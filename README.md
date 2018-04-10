# Rematch

Pattern matching. See `@doc @match` for syntax.

## Differences from [Match.jl](https://github.com/kmsquire/Match.jl)

* If no branches are matched, throws `MatchFailure` instead of returning nothing.
* Matching against a struct with the wrong number of fields produces an error instead of silently failing.
* Repeated variables are handled correctly ie `@match (1,2) begin (x,x) => :ok end` fails.
* The syntax for guards is `x where x > 1` instead of `x, if x > 1 end` and can occur anywhere in a pattern.
* No support (yet) for matching `Regex` or `UnitRange`.
* No support (yet) for matching against multidimensional arrays - all array patterns use linear indexing.
