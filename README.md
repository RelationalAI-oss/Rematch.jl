# Rematch

[![Build Status](https://travis-ci.org/RelationalAI-oss/Rematch.jl.svg?branch=master)](https://travis-ci.org/RelationalAI-oss/Rematch.jl)

Pattern matching.

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

``` julia
@match pattern = value
```

If value matches pattern, binds variables and returns `value`. Otherwise, throws MatchFailure.

``` julia
@match value begin
    pattern1 => result1
    pattern2 => result2
    ...
end
```

Returns `result` for the first matching pattern. If there are no matching patterns, throws MatchFailure.

## Patterns

* `_` matches anything
* `foo` matches anything, binds value to `foo`
* `Foo(x,y,z)` matches structs of type `Foo` with fields matching `x,y,z`
* `[x,y,z]` matches `AbstractArray`s with 3 entries matching `x,y,z`
* `(x,y,z)` matches `Tuple`s with 3 entries matching `x,y,z`
* `[x,y...,z]` matches `AbstractArray`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
* `(x,y...,z)` matches `Tuple`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
* `_::T` matches any subtype (`isa`) of T
* `x || y` matches values which match either `x` or `y` (only variables which exist in both branches will be bound)
* `x && y` matches values which match both `x` and `y`
* `x where condition` matches only if `condition` is true (`condition` may use any variables that occur earlier in the pattern eg `(x, y, z where x + y > z)`)
* Anything else is treated as a constant and tested for equality

Patterns can be nested arbitrarily.

Repeated variables only match if they are `==` eg `(x,x)` matches `(1,1)` but not `(1,2)`.

## Differences from [Match.jl](https://github.com/kmsquire/Match.jl)

* If no branches are matched, throws `MatchFailure` instead of returning nothing.
* Matching against a struct with the wrong number of fields produces an error instead of silently failing.
* Repeated variables are handled correctly ie `@match (1,2) begin (x,x) => :ok end` fails.
* The syntax for guards is `x where x > 1` instead of `x, if x > 1 end` and can occur anywhere in a pattern.
* No support (yet) for matching `Regex` or `UnitRange`.
* No support (yet) for matching against multidimensional arrays - all array patterns use linear indexing.
