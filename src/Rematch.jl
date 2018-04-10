module Rematch

using Delve.Util
import MacroTools
import MacroTools: @capture

struct MatchFailure
    value
end

function assert_num_fields(::Type{T}, matched::Integer) where {T}
    actual = length(fieldnames(T))
    @assert actual == matched "Tried to match $matched fields of $actual field struct $T"
end

function handle_destruct_fields(value::Symbol, pattern, subpatterns, len, get::Symbol, bound::Set{Symbol}; allow_splat=true)
    # NOTE we assume `len` is cheap
    fields = []
    seen_splat = false
    for (i,subpattern) in enumerate(subpatterns)
        if (subpattern isa Expr) && (subpattern.head == :(...))
            @assert allow_splat && !seen_splat "Too many ... in pattern $pattern"
            @assert length(subpattern.args) == 1
            seen_splat = true
            push!(fields, (:($i:($len-$(length(subpatterns)-i))), subpattern.args[1]))
        elseif seen_splat
            push!(fields, (:($len-$(length(subpatterns)-i)), subpattern))
        else
            push!(fields, (i, subpattern))
        end
    end
    Expr(:&&,
        if seen_splat
            :($len >= $(length(subpatterns)-1))
        else
            :($len == $(length(subpatterns)))
        end,
        @splice (i, (field, subpattern)) in enumerate(fields) quote
            $(Symbol("$(value)_$i")) = $get($value, $field)
            $(handle_destruct(Symbol("$(value)_$i"), subpattern, bound))
        end)
end

function handle_destruct(value::Symbol, pattern, bound::Set{Symbol})
    if pattern == :(_)
        # wildcard
        true
    elseif !(pattern isa Expr || pattern isa Symbol) ||
           pattern == :nothing ||
           @capture(pattern, _quote_macrocall) ||
           @capture(pattern, Symbol(_))
        # constant
        # TODO do we have to be careful about QuoteNode etc?
        quote
            $value == $pattern
        end
    elseif @capture(pattern, subpattern_Symbol)
        # variable
        # if the pattern doesn't match, we don't want to set the variable
        # so for now just set a temp variable
        our_sym = Symbol("variable_$pattern")
        if pattern in bound
            # already bound, check that this value matches
            quote
                $our_sym == $value
            end
        else
            # bind
            push!(bound, pattern)
            quote
                $our_sym = $value;
                true
            end
        end
    elseif @capture(pattern, subpattern1_ || subpattern2_)
        # disjunction
        # need to only bind variables which exist in both branches
        bound1 = copy(bound)
        bound2 = copy(bound)
        body1 = handle_destruct(value, subpattern1, bound1)
        body2 = handle_destruct(value, subpattern2, bound2)
        union!(bound, intersect(bound1, bound2))
        quote
            $body1 || $body2
        end
    elseif @capture(pattern, _where)
        # guard
        @assert length(pattern.args) == 2
        subpattern = pattern.args[1]
        guard = pattern.args[2]
        quote
            $(handle_destruct(value, subpattern, bound)) &&
            let $(bound...)
                # bind variables locally so they can be used in the guard
                $(@splice variable in bound quote
                    $(esc(variable)) = $(Symbol("variable_$variable"))
                end)
                $(esc(guard))
            end
        end
    elseif @capture(pattern, T_Symbol(subpatterns__))
        # struct
        quote
            assert_num_fields($(esc(T)), $(length(subpatterns)))
            typeof($value) == $(esc(T)) &&
            $(handle_destruct_fields(value, pattern, subpatterns, length(subpatterns), :getfield, bound; allow_splat=false))
        end
    elseif @capture(pattern, (subpatterns__,))
        # tuple
        quote
            ($value isa Tuple) &&
            $(handle_destruct_fields(value, pattern, subpatterns, :(length($value)), :getindex, bound; allow_splat=true))
        end
    elseif @capture(pattern, [subpatterns__])
        # array
        quote
            ($value isa AbstractArray) &&
            $(handle_destruct_fields(value, pattern, subpatterns, :(length($value)), :getindex, bound; allow_splat=true))
        end
    elseif @capture(pattern, subpattern_::T_)
        # typeassert
        quote
            ($value isa $(esc(T))) &&
            $(handle_destruct(value, subpattern, bound))
        end
    else
        error("Unrecognized pattern syntax: $pattern")
    end
end

function handle_capture(value, pattern)
    bound = Set{Symbol}()
    quote
        $(handle_destruct(:value, pattern, bound)) &&
        begin
            # export bindings
            $(@splice variable in bound quote
                $(esc(variable)) = $(Symbol("variable_$variable"))
            end)
            true
        end
    end
end

function handle_match_eq(expr)
    if @capture(expr, pattern_ = value_)
        quote
            value = $(esc(value))
            $(handle_capture(:value, pattern)) || throw(MatchFailure(value))
            value
        end
    else
        error("Unrecognized match syntax: $expr")
    end
end

function handle_match_case(value, case, tail)
    if @capture(case, pattern_ => result_)
        quote
            if $(handle_capture(value, pattern))
                $(esc(result))
            else
                $tail
            end
        end
    else
        error("Unrecognized case syntax: $case")
    end
end

function handle_match_cases(value, match)
    tail = :(throw(MatchFailure(value)))
    if @capture(match, begin cases__ end)
        for case in reverse(cases)
            tail = handle_match_case(:value, case, tail)
        end
    else
        error("Unrecognized match syntax: $value $match")
    end
    quote
        value = $(esc(value))
        $tail
    end
end

"""
    @match pattern = value

If `value` matches `pattern`, bind variables and return `value`. Otherwise, throw `MatchFailure`.
"""
macro match(expr)
    handle_match_eq(expr)
end

"""
    @match value begin
        pattern1 => result1
        pattern2 => result2
        ...
    end

Return `result` for the first matching `pattern`. If there are no matches, throw `MatchFailure`.
"""
macro match(value, cases)
    handle_match_cases(value, cases)
end

"""
Patterns:

  * `_` matches anything
  * `foo` matches anything, binds value to `foo`
  * `Foo(x,y,z)` matches structs of type `Foo` with fields matching `x,y,z`
  * `[x,y,z]` matches `AbstractArray`s with 3 entries matching `x,y,z`
  * `(x,y,z)` matches `Tuple`s with 3 entries matching `x,y,z`
  * `[x,y...,z]` matches `AbstractArray`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
  * `(x,y...,z)` matches `Tuple`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
  * `_::T` matches any subtype (`isa`) of T
  * `x || y` matches `x` or `y` (only variables which exist in both branches will be bound)
  * `x where condition` matches only if `condition` is true (`condition` may use any variables that occur earlier in the pattern eg `(x, y, z where x + y > z)`)
  * Anything else is treated as a constant and tested for equality

Patterns can be nested arbitrarily.

Repeated variables only match if they are `==` eg `(x,x)` matches `(1,1)` but not `(1,2)`.
"""
:(@match)

export @match

end
