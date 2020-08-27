# Rematch

[![Build Status](https://travis-ci.org/RelationalAI-oss/Rematch.jl.svg?branch=master)](https://travis-ci.org/RelationalAI-oss/Rematch.jl)

Pattern matching.

`Rematch.jl` provides a syntax sugar for matching julia values against syntactic
patterns. The `@match` macro expands a pattern-matching syntax into a series of
if-elses that check the types and structure of the provided value, allowing you
to more simply write checks that describe your intent.

``` julia
julia> using Rematch

julia> struct Foo
           x::Int64
           y::String
       end

julia> f(x) = @match x begin
           _::String => :string
           [a,a,a] => (:all_the_same, a)
           [a,bs...,c] => (:at_least_2, a, bs, c)
           Foo(x, "foo") where x > 1 => :foo
       end
f (generic function with 1 method)

julia> f("foo")
:string

julia> f([1,1,1])
(:all_the_same, 1)

julia> f([1,1])
(:at_least_2, 1, Int64[], 1)

julia> f([1,2,3,4])
(:at_least_2, 1, [2, 3], 4)

julia> f([1])
ERROR: Rematch.MatchFailure([1])
Stacktrace:
 [1] macro expansion at /home/jamie/.julia/v0.6/Rematch/src/Rematch.jl:173 [inlined]
 [2] f(::Array{Int64,1}) at ./REPL[3]:1

julia> f(Foo(2, "foo"))
:foo

julia> f(Foo(0, "foo"))
ERROR: Rematch.MatchFailure(Foo(0, "foo"))
Stacktrace:
 [1] macro expansion at /home/jamie/.julia/v0.6/Rematch/src/Rematch.jl:173 [inlined]
 [2] f(::Foo) at ./REPL[13]:1

julia> f(Foo(2, "not a foo"))
ERROR: Rematch.MatchFailure(Foo(2, "not a foo"))
Stacktrace:
 [1] macro expansion at /home/jamie/.julia/v0.6/Rematch/src/Rematch.jl:173 [inlined]
 [2] f(::Foo) at ./REPL[13]:1
```

## Usage

### Assignment Syntax
``` julia
@match pattern = value
```

If value matches pattern, binds variables and returns `value`. Otherwise, throws `MatchFailure`.

After evaluation, any variable names used within `pattern` will be bound as new variables in the enclosing scope. For example:
```julia
julia> @match Foo(x,2) = Foo(1,2)
Foo(1,2)

julia> x
1
```

### Case Syntax

``` julia
@match value begin
    pattern1 => result1
    pattern2 => result2
    ...
end
```

Returns `result` for the first matching pattern. If there are no matching patterns, throws `MatchFailure`.

Note that unlike the _assignment syntax_, this does not create any variable bindings outside the match macro.

## Patterns

* `_` matches anything
* `foo` matches anything, binds value to `foo`
* `~Foo(x,y,z)` calls the extractor function `unapply_Foo(value)` which returns a tuple matching `(x,y,z)`
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
* Expressions can be interpolated in as constants via standard interpolation syntax `$(x)`

Patterns can be nested arbitrarily.

Repeated variables only match if they are equal (`==`). For example `(x,x)` matches `(1,1)` but not `(1,2)`.

### Extractors

Patterns can use _extractor functions_ (also known as _active patterns_).
These are just any function that takes a value to match and returns either `nothing` (indicating match failure)
or a tuple that decomposes the value. The tuple is then matched against other patterns.

An extractor function must take one argument--the value to be matched against--and should return 
a `Union{Tuple{T,...}, Nothing}`.  Returning `nothing` indicates the extractor does not match.
If a tuple is returned, the components are matched against the subpatterns of the pattern.
Extractor functions should have names of the form `unapply_X`, for some name `X`.

Extractor patterns are written `~X(p1, p2, ...)`, where `unapply_X` is the name of the extractor function
and `p1`, `p2`, etc., are subpatterns to match against the returned tuple.

For example, to destruct an array into its head and tail, one could write the following function:

```julia
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
```

Here's an extractor that extracts the polar coordinates of a Cartesian point:

```julia
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
```

Extractors can even be higher-order.

```julia
function unapply_Re(r::Regex)
    x -> begin
        m = match(r, x)
        if m == nothing
            return nothing
        else
            return tuple(m.captures...)
        end
    end
end

@match "abc123def" begin
    ~Re(r"(\w+?)(\d+)(\w+)")(a,x,d) =>
        begin
            @assert a == "abc"
            @assert x == "123"
            @assert d == "def"
        end
    _ => @assert false
end


## Differences from [Match.jl](https://github.com/kmsquire/Match.jl)

This package was branched from the original [Match.jl](https://github.com/kmsquire/Match.jl). It now differs in several ways:

* If no branches are matched, throws `MatchFailure` instead of returning nothing.
* Matching against a struct with the wrong number of fields produces an error instead of silently failing.
* Repeated variables require equality, ie `@match (1,2) begin (x,x) => :ok end` fails.
* The syntax for guards is `x where x > 1` instead of `x, if x > 1 end` and can occur anywhere in a pattern.
* Structs can be matched by field-names, allowing partial matches: `@match Foo(1,2) begin Foo(y=2) => :ok end` returns `:ok`.
* Patterns support interpolation, ie `let x=1; @match ($x,$(x+1)) = (1,2); end` is a match.
* Extractor functions can be used in patterns.
* No support (yet) for matching `Regex` or `UnitRange`.
* No support (yet) for matching against multidimensional arrays - all array patterns use linear indexing.
