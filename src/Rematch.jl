module Rematch

import MacroTools
import MacroTools: @capture

macro splice(iterator, body)
  @assert iterator.head == :call
  @assert iterator.args[1] == :in
  Expr(:..., :(($(esc(body)) for $(esc(iterator.args[2])) in $(esc(iterator.args[3])))))
end

struct MatchFailure <: Exception
    value
end

function assert_num_fields(::Type{T}, matched::Integer) where {T}
    actual = fieldcount(T)
    @assert actual == matched "Tried to match $matched fields of $actual field struct $T"
end

"""
Statically get the fieldcount of a type. Useful to avoid runtime calls to
fieldcount.
"""
@generated function evaluated_fieldcount(t::Type{T}) where T
    # TODO(nathan.daly) Does this need to be @generated, or is this function even necessary?
    fieldcount(T)
end

function handle_destruct_fields(value::Symbol, pattern, subpatterns, len, get::Symbol, bound::Set{Symbol}, asserts::Vector{Expr}; allow_splat=true)
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
        elseif (subpattern isa Expr) && (subpattern.head == :kw)
            field_symbol = Meta.quot(subpattern.args[1])
            push!(fields, (field_symbol, subpattern.args[2]))
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
            $(handle_destruct(Symbol("$(value)_$i"), subpattern, bound, asserts))
        end)
end

function handle_destruct(value::Symbol, pattern, bound::Set{Symbol}, asserts::Vector{Expr})
    if pattern == :(_)
        # wildcard
        true
    elseif !(pattern isa Expr || pattern isa Symbol) ||
           pattern == :nothing ||
           @capture(pattern, _quote_macrocall) ||
           @capture(pattern, Symbol(_))
        # constant
        quote
            $value == $pattern
        end
    elseif (pattern isa Expr && pattern.head == :$)
        # interpolated value
        quote
            $value == $(esc(pattern.args[1]))
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
    elseif @capture(pattern, subpattern1_ || subpattern2_) || (@capture(pattern, f_(subpattern1_, subpattern2_)) && f == :|)
        # disjunction
        # need to only bind variables which exist in both branches
        bound1 = copy(bound)
        bound2 = copy(bound)
        body1 = handle_destruct(value, subpattern1, bound1, asserts)
        body2 = handle_destruct(value, subpattern2, bound2, asserts)
        union!(bound, intersect(bound1, bound2))
        quote
            $body1 || $body2
        end
    elseif @capture(pattern, subpattern1_ && subpattern2_) || (@capture(pattern, f_(subpattern1_, subpattern2_)) && f == :&)
        # conjunction
        body1 = handle_destruct(value, subpattern1, bound, asserts)
        body2 = handle_destruct(value, subpattern2, bound, asserts)
        quote
            $body1 && $body2
        end
    elseif @capture(pattern, _where)
        # guard
        @assert length(pattern.args) == 2
        subpattern = pattern.args[1]
        guard = pattern.args[2]
        quote
            $(handle_destruct(value, subpattern, bound, asserts)) &&
            let $(bound...)
                # bind variables locally so they can be used in the guard
                $(@splice variable in bound quote
                    $(esc(variable)) = $(Symbol("variable_$variable"))
                end)
                $(esc(guard))
            end
        end
    elseif @capture(pattern, T_(subpatterns__))
        # struct      
        len = length(subpatterns)
        named_fields = [pat.args[1] for pat in subpatterns if (pat isa Expr) && pat.head == :kw]
        named_count = length(named_fields)
        @assert named_count == length(unique(named_fields)) "Pattern $pattern has duplicate named arguments ($(named_fields))."
        @assert named_count == 0 || named_count == len "Pattern $pattern mixes named and positional arguments."
        if named_count == 0
            # Pattern uses positional arguments to refer to fields e.g. Foo(0, true)
            expected_fieldcount = gensym("$(T)_expected_fieldcount")
            actual_fieldcount = gensym("$(T)_actual_fieldcount")
            push!(asserts, quote
                if typeof($(esc(T))) <: Function
                    throw(LoadError("Attempted to match on a function", @__LINE__, AssertionError("Incorrect match usage")))
                end
                if !(isstructtype(typeof($(esc(T)))) || issabstracttype(typeof($(esc(T)))))
                    throw(LoadError("Attempted to match on a pattern that is not a struct", @__LINE__, AssertionError("Incorrect match usage")))
                end
                # This assertion is necessary:
                # If $expected_fieldcount < $actual_fieldcount, to catch missing fields.
                # If $expected_fieldcount > $actual_fieldcount, to avoid a BoundsError.
                $expected_fieldcount = evaluated_fieldcount($(esc(T)))
                $actual_fieldcount = $(esc(len))
                if $expected_fieldcount != $actual_fieldcount
                    error("Pattern field count is $($actual_fieldcount) expected $($expected_fieldcount)")
                end
                 
            end)
        else
            # Pattern uses named arguments to refer to fields e.g. Foo(x=0, y=true)
            # Could assert that the expected field names are a subset of the actual field names.
            # Can omit the assertion because if the field doesn't exist getfield() will fail with "type $T has no field $field".
        end
        quote
            # I would prefer typeof($value) == $(esc(T)) but this doesn't convey type information in Julia 0.6
            $value isa $(esc(T)) &&
            $(handle_destruct_fields(value, pattern, subpatterns, length(subpatterns), :getfield, bound, asserts; allow_splat=false))
        end
    elseif @capture(pattern, (subpatterns__,))
        # tuple
        quote
            ($value isa Tuple) &&
            $(handle_destruct_fields(value, pattern, subpatterns, :(length($value)), :getindex, bound, asserts; allow_splat=true))
        end
    elseif @capture(pattern, [subpatterns__])
        # array
        quote
            ($value isa AbstractArray) &&
            $(handle_destruct_fields(value, pattern, subpatterns, :(length($value)), :getindex, bound, asserts; allow_splat=true))
        end
    elseif @capture(pattern, subpattern_::T_)
        # typeassert
        quote
            ($value isa $(esc(T))) &&
            $(handle_destruct(value, subpattern, bound, asserts))
        end
    else
        error("Unrecognized pattern syntax: $pattern")
    end
end

function handle_match_eq(expr)
    if @capture(expr, pattern_ = value_)
        asserts = Expr[]
        bound = Set{Symbol}()
        body = handle_destruct(:value, pattern, bound, asserts)
        quote
            $(asserts...)
            value = $(esc(value))
            $body || throw(MatchFailure(value))
            $(@splice variable in bound quote
                $(esc(variable)) = $(Symbol("variable_$variable"))
            end)
            value
        end
    else
        error("Unrecognized match syntax: $expr")
    end
end

function handle_match_case(value, case, tail, asserts)
    if @capture(case, pattern_ => result_)
        bound = Set{Symbol}()
        body = handle_destruct(:value, pattern, bound, asserts)
        quote
            if $body
                let $(bound...)
                    # export bindings
                    $(@splice variable in bound quote
                      $(esc(variable)) = $(Symbol("variable_$variable"))
                      end)
                    $(esc(result))
                end
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
        asserts = Expr[]
        for case in reverse(cases)
            tail = handle_match_case(:value, case, tail, asserts)
        end
        quote
            $(asserts...)
            value = $(esc(value))
            $tail
        end
    else
        error("Unrecognized match syntax: $value $match")
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
  * `Foo(y=1)` matches structs of type `Foo` whose `y` field equals `1`
  * `[x,y,z]` matches `AbstractArray`s with 3 entries matching `x,y,z`
  * `(x,y,z)` matches `Tuple`s with 3 entries matching `x,y,z`
  * `[x,y...,z]` matches `AbstractArray`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
  * `(x,y...,z)` matches `Tuple`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
  * `_::T` matches any subtype (`isa`) of T
  * `x || y` matches values which match either `x` or `y` (only variables which exist in both branches will be bound)
  * `x && y` matches values which match both `x` and `y`
  * `x where condition` matches only if `condition` is true (`condition` may use any variables that occur earlier in the pattern eg `(x, y, z where x + y > z)`)
  * Anything else is treated as a constant and tested for equality
  * Expressions can be interpolated in as constants via standard interpolation syntax `\$(x)`

Patterns can be nested arbitrarily.

Repeated variables only match if they are equal (`==`). For example `(x,x)` matches `(1,1)` but not `(1,2)`.
"""
:(@match)

export @match

end
