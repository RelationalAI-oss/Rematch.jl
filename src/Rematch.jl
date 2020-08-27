module Rematch

import MacroTools
import MacroTools: @capture
import Compat: occursin, fieldcount

macro splice(iterator, body)
  @assert iterator.head == :call
  @assert iterator.args[1] == :in
  Expr(:..., :(($(esc(body)) for $(esc(iterator.args[2])) in $(esc(iterator.args[3])))))
end

struct MatchFailure
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

"""
    handle_destruct_fields(value, pattern, subpatterns, len, get, bound, asserts; allow_splat=true)

Destruct a tuple, vector, or struct `value` with the given `pattern`.
Returns a boolean expression that evaluates to true if the pattern matches.
Variables bound in the pattern are added to the `bound` set.
Assertions are added to the `asserts` vector.

The value is indexed by `get` from 1 to `len` and element i is matched against `subpattern` i.
If `allow_splat` is true, one `...` is allowed among the subpatterns.
"""
function handle_destruct_fields(
    value::Symbol,
    pattern,
    subpatterns,
    len,
    get::Symbol,
    bound::Set{Symbol},
    asserts::Vector{Expr};
    allow_splat=true
)
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

    return Expr(:&&,
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

"""
handle_destruct(value, pattern, bound, asserts)

Destruct `value` with the given `pattern`.

The pattern is compiled to a boolean expression which evaluates to `true` if the
pattern matches. Variables bound in the pattern are added to the `bound` set.
Assertions are added to the `asserts` vector.
"""
function handle_destruct(value::Symbol, pattern, bound::Set{Symbol}, asserts::Vector{Expr})
    if pattern == :(_)
        # wildcard
        return true
    elseif !(pattern isa Expr || pattern isa Symbol) ||
           pattern == :nothing ||
           @capture(pattern, _quote_macrocall) ||
           @capture(pattern, Symbol(_))
        # constant
        return quote
            $value == $pattern
        end
    elseif (pattern isa Expr && pattern.head == :$)
        # interpolated value
        return quote
            $value == $(esc(pattern.args[1]))
        end
    elseif @capture(pattern, subpattern_Symbol)
        # variable

        # if the pattern doesn't match, we don't want to set the variable
        # so for now just set a temp variable
        our_sym = Symbol("variable_$pattern")

        if pattern in bound
            # already bound, check that this value matches
            return quote
                $our_sym == $value
            end
        else
            # bind
            push!(bound, pattern)
            return quote
                $our_sym = $value;
                true
            end
        end
    elseif @capture(pattern, subpattern1_ || subpattern2_) ||
          (@capture(pattern, f_(subpattern1_, subpattern2_)) && f == :|)
        # disjunction

        # need to only bind variables which exist in both branches
        bound1 = copy(bound)
        bound2 = copy(bound)

        body1 = handle_destruct(value, subpattern1, bound1, asserts)
        body2 = handle_destruct(value, subpattern2, bound2, asserts)
        union!(bound, intersect(bound1, bound2))

        return quote
            $body1 || $body2
        end
    elseif @capture(pattern, subpattern1_ && subpattern2_) ||
          (@capture(pattern, f_(subpattern1_, subpattern2_)) && f == :&)
        # conjunction

        body1 = handle_destruct(value, subpattern1, bound, asserts)
        body2 = handle_destruct(value, subpattern2, bound, asserts)

        return quote
            $body1 && $body2
        end
    elseif @capture(pattern, _where)
        # guard
        @assert length(pattern.args) == 2

        subpattern = pattern.args[1]
        guard = pattern.args[2]

        return quote
            $(handle_destruct(value, subpattern, bound, asserts)) &&
            let $(bound...)
                # bind variables locally so they can be used in the guard
                $(@splice variable in bound quote
                    $(esc(variable)) = $(Symbol("variable_$variable"))
                end)
                $(esc(guard))
            end
        end
    elseif @capture(pattern, ~p_) && @capture(p, f_(subpatterns__))
        # Extractor call.
        # Extractor calls must begin with `~`.

        result = gensym("unapply")
        len = length(subpatterns)

        function replace_call(f)
            if f isa Symbol
                return Symbol("unapply_$f")
            elseif @capture(f, g_(args__))
                h = replace_call(g)
                if h == nothing
                    return nothing
                end
                return Expr(:call, replace_call(g), args...)
            else
                return nothing
            end
        end

        f = replace_call(f)

        if f === nothing
            error("Extractor pattern could not be parsed: should be a call to a named function.")
        end

        # The function should take one argument and return a `Union{Tuple{T,...}, Nothing}`.
        # If the argument does not match, nothing should be returned.
        # If the argument does match, a tuple should be returned, which is then matched against
        # the subpatterns.
        if len == 0
            # If there are no subpatterns, the result value is just checked for
            # nothingness.
            return quote
                $(esc(f))($value) !== nothing
            end
        else
            # If there are subpatterns, the result value is matched against a tuple pattern.
            return quote
                begin
                    $result = $(esc(f))($value)
                    $result !== nothing &&
                    ($result isa Tuple) &&
                    $(handle_destruct_fields(
                        result,
                        pattern,
                        subpatterns,
                        :(length($result)),
                        :getindex,
                        bound,
                        asserts;
                        allow_splat=true
                    ))
                end
            end
        end
    elseif @capture(pattern, T_(subpatterns__))
        # Struct.

        # Since extractors and struct patterns have similar syntax, we require structs to
        # be either symbols or qualified names. We then eval the struct name to check if
        # it is a DataType. Extractors should be Functions instead, and can have arbitrary
        # syntax.

        len = length(subpatterns)

        named_fields = [pat.args[1] for pat in subpatterns
                                    if (pat isa Expr) && pat.head == :kw]
        named_count = length(named_fields)

        @assert named_count == length(unique(named_fields))
            "Pattern $pattern has duplicate named arguments ($(named_fields))."
        @assert named_count == 0 || named_count == len
            "Pattern $pattern mixes named and positional arguments."

        if named_count == 0
            # Pattern uses positional arguments to refer to fields e.g. Foo(0, true)
            expected_fieldcount = gensym("$(T)_expected_fieldcount")
            actual_fieldcount = gensym("$(T)_actual_fieldcount")
            push!(asserts, quote
                t = $(esc(T))
                if typeof(t) <: Function
                    throw(LoadError("Attempted to match on a function",
                                    @__LINE__, AssertionError("Incorrect match usage")))
                end
                if !(isstructtype(typeof(t)) || issabstracttype(typeof(t)))
                    throw(LoadError("Attempted to match on a type that is not a struct",
                                    @__LINE__, AssertionError("Incorrect match usage")))
                end
                # This assertion is necessary:
                # If $expected_fieldcount < $actual_fieldcount, to catch missing fields.
                # If $expected_fieldcount > $actual_fieldcount, to avoid a BoundsError.
                $expected_fieldcount = evaluated_fieldcount(t)
                $actual_fieldcount = $(esc(len))
                if $expected_fieldcount != $actual_fieldcount
                    error("Pattern field count is $($actual_fieldcount), ",
                            "expected $($expected_fieldcount)")
                end

            end)
        else
            # Pattern uses named arguments to refer to fields e.g. Foo(x=0, y=true)
            # Could assert that the expected field names are a subset of the actual
            # field names. Can omit the assertion because if the field doesn't exist
            # getfield() will fail with "type $T has no field $field".
        end

        return quote
            # I would prefer typeof($value) == $(esc(T)) but this doesn't convey type
            # information in Julia 0.6
            $value isa $(esc(T)) &&
            $(handle_destruct_fields(
                value,
                pattern,
                subpatterns,
                length(subpatterns),
                :getfield,
                bound,
                asserts;
                allow_splat=false
            ))
        end
    elseif @capture(pattern, (subpatterns__,))
        # tuple
        return quote
            ($value isa Tuple) &&
            $(handle_destruct_fields(
                value,
                pattern,
                subpatterns,
                :(length($value)),
                :getindex,
                bound,
                asserts;
                allow_splat=true
            ))
        end
    elseif @capture(pattern, [subpatterns__])
        # array
        return quote
            ($value isa AbstractArray) &&
            $(handle_destruct_fields(
                value,
                pattern,
                subpatterns,
                :(length($value)),
                :getindex,
                bound,
                asserts;
                allow_splat=true
            ))
        end
    elseif @capture(pattern, subpattern_::T_)
        # typeassert
        return quote
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
        return quote
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

        return quote
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
    if @capture(match, begin cases__ end)
        tail = :(throw(MatchFailure(value)))
        asserts = Expr[]

        for case in reverse(cases)
            tail = handle_match_case(:value, case, tail, asserts)
        end

        return quote
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

If `value` matches `pattern`, bind variables and return `value`.
Otherwise, throw `MatchFailure`.
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

Return `result` for the first matching `pattern`.
If there are no matches, throw `MatchFailure`.
"""
macro match(value, cases)
    handle_match_cases(value, cases)
end

"""
Patterns:

  * `_` matches anything
  * `foo` matches anything, binds value to `foo`
  * `~Foo(x,y,z)` calls the extractor function `unapply_Foo(value)` which returns a tuple
    matching `(x,y,z)`
  * `Foo(x,y,z)` matches structs of type `Foo` with fields matching `x,y,z`
  * `Foo(y=1)` matches structs of type `Foo` whose `y` field equals `1`
  * `[x,y,z]` matches `AbstractArray`s with 3 entries matching `x,y,z`
  * `(x,y,z)` matches `Tuple`s with 3 entries matching `x,y,z`
  * `[x,y...,z]` matches `AbstractArray`s with at least 2 entries, where `x` matches
    the first entry, `z` matches the last entry and `y` matches the remaining entries.
  * `(x,y...,z)` matches `Tuple`s with at least 2 entries, where `x` matches the first
    entry, `z` matches the last entry and `y` matches the remaining entries.
  * `_::T` matches any subtype (`isa`) of T
  * `x || y` matches values which match either `x` or `y` (only variables which exist in
    both branches will be bound)
  * `x && y` matches values which match both `x` and `y`
  * `x where condition` matches only if `condition` is true (`condition` may use any
    variables that occur earlier in the pattern eg `(x, y, z where x + y > z)`)
  * Anything else is treated as a constant and tested for equality
  * Expressions can be interpolated in as constants via standard interpolation
    syntax `\$(x)`

Patterns can be nested arbitrarily.

Repeated variables only match if they are equal (`==`).
For example `(x,x)` matches `(1,1)` but not `(1,2)`.

Extractors:

An extractor function must take one argument--the value to be matched against--and should
return either a tuple of values or `nothing`. Returning `nothing` indicates the extractor
does not match. For example to destruct an array into its head and tail:

    function unapply_Cons(xs)
        if isempty(xs)
            return nothing
        else
            return ([xs[1], xs[2:end]])
        end
    end

    @match [1,2,3] begin
        ~Cons(x, xs) => @assert x == 1 && xs == [2,3]
    end

Or here's one to extract the polar coordinates of a point:

    function unapply_Polar(p)
        @match p begin
            (x, y) =>
                begin
                    r = sqrt(x^2+y^2)
                    theta = atan(y, x)
                    return (r, theta)
                end
            _ => return nothing
        end
    end

    @match (1,1) begin
        ~Polar(r, theta) => @assert r == sqrt(2) && theta == pi/4
    end
"""
:(@match)

export @match, @pattern

end
