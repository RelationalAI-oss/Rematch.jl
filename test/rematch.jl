using Compat.Test
using Compat.InteractiveUtils
using Rematch
import Rematch: MatchFailure

if VERSION < v"0.7.0-DEV"
    AbstractRange = Range
end

assertion_error = (VERSION >= v"0.7.0-DEV") ? LoadError : AssertionError

# Note we use `@eval` to allow defining a struct within a @testset
# (https://github.com/JuliaLang/julia/issues/23794)

@testset "Basic Matching" begin
    @eval struct Foo
        x
        y
    end

    @eval begin
        # test basic match
        let x = nothing
            @test (@match Foo(x,2) = Foo(1,2)) == Foo(1,2)
            @test x == 1
        end

        # variables not bound if match fails
        let x = nothing
            @test_throws MatchFailure @match Foo(x, 3) = Foo(1,2)
            @test x == nothing
        end

        # doesn't overwrite variables in outer scope
        let x = nothing
            @test (@match Foo(1,2) begin
                Foo(x,2) => x
            end) == 1
            @test x == nothing
        end

        # variables not bound if guard fails
        let x = nothing
            @test_throws MatchFailure @match Foo(1,2) begin
              Foo(x, 2) where x != 1 => :ok
            end
            @test x == nothing
        end
    end
end

@testset "Match using extractors" begin
    function unapply_sub1(x) tuple(x+1) end

    # sub1(4) == 3
    let x = nothing
        @test (@match 3 begin ~sub1(x) => x end) == 4
        @test x == nothing
    end

    function unapply_Cons(xs)
        isempty(xs) ? nothing : (xs[1], xs[2:end])
    end

    # ~Cons(1, ~Cons(4, (~Cons(9, []))) == [1,4,9]
    let a = nothing
        b = nothing
        c = nothing

        @test (@match [1,4,9] begin
           ~Cons(a, ~Cons(b, ~Cons(c, []))) => (a,b,c)
        end) == (1,4,9)

        @test a == nothing
        @test b == nothing
        @test c == nothing
    end

    @match [1,2,3] begin
        ~Cons(x, xs) =>
            begin
                @test x == 1
                @test xs == [2,3]
            end
    end

    function unapply_Snoc(xs)
        isempty(xs) ? nothing : (xs[1:end-1], xs[end])
    end

    let a = nothing
        b = nothing
        c = nothing

        @test (@match [1,4,9] begin
            ~Snoc(~Snoc(~Snoc([], c), b), a) => (a,b,c)
        end) == (9,4,1)

        @test a == nothing
        @test b == nothing
        @test c == nothing
    end

    @match [1,2,3] begin
        ~Snoc(xs, x) =>
            begin
                @test x == 3
                @test xs == [1,2]
            end
    end

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
        ~Polar(r, theta) =>
           begin
               @test r == sqrt(2)
               @test theta == pi/4
           end
    end

    # A more complex extractor.
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
                @test a == "abc"
                @test x == "123"
                @test d == "def"
            end
        _ => @test false
    end

    @match "abc123def" begin
        ~Re(r"(\d+)")(x) => @test x == "123"
        _ => @test false
    end

    @match "abc123def" begin
        ~Re(r"\d+")() => @test true
        _ => @test false
    end
end

@testset "Red-black trees with extractors" begin
    # Okasaki-style red-black trees
    @enum Color R B
    @eval abstract type Tree end
    @eval struct E <: Tree end
    @eval struct T <: Tree
        color::Color
        a::Tree
        x
        b::Tree
    end

    # Extractor for red nodes
    function unapply_Red(t::Tree)
        @match t begin
            # Use color == B since we can't match against an enum value R
            T(color, a, x, b) where (color == R) => (a, x, b)
            _ => nothing
        end
    end

    # Extractor for black nodes
    function unapply_Blk(t::Tree)
        @match t begin
            # Use color == B since we can't match against an enum value B
            T(color, a, x, b) where (color == B) => (a, x, b)
            _ => nothing
        end
    end

    # Constructors
    function Red(a, x, b) T(R, a, x, b) end
    function Blk(a, x, b) T(B, a, x, b) end

    function member(x, t::Tree)
        @match t begin
            E() => false
            T(_, a, y, b) where (x < y) => member(x, a)
            T(_, a, y, b) where (x > y) => member(x, b)
            _ => true
        end
    end

    function insert(x, s::Tree)
        function ins(t)
            @match t begin
                E() => Red(E(), x, E())
                T(color, a, y, b) where (x < y) => balance(T(color, ins(a), y, b))
                T(color, a, y, b) where (x > y) => balance(T(color, a, y, ins(b)))
                t => t
            end
        end

        @match T(_, a, y, b) = ins(s)

        Blk(a, y, b)
    end

    function balance(t::T)
        @match t begin
            ( ~Blk(~Red(~Red(a, x, b), y, c), z, d) ||
              ~Blk(~Red(a, x, ~Red(b, y, c)), z, d) ||
              ~Blk(a, x, ~Red(~Red(b, y, c), z, d)) ||
              ~Blk(a, x, ~Red(b, y, ~Red(c, z, d))) ) => Red(Blk(a, x, b), y, Blk(c, z, d))
            t => t
        end
    end

    function height(t::Tree)
        @match t begin
            E() => 0
            T(_, a, x, b) => max(height(a), height(b)) + 1
        end
    end

    @test member(3, insert(3, E()))

    n = 100

    t = E()
    for x in 1:n
        t = insert(x, t)
    end

    theoretical_max_height = 2 * floor(log(2, n+1))
    @test height(t) <= theoretical_max_height

    for x in 1:n
        @test member(x, t)
    end
end

@testset "Match Struct by field names" begin
    # match one struct field by name
    let x = nothing
        x1 = nothing
        @test (@match Foo(1,2) begin
               Foo(x=x1) => x1
        end) == 1
        @test x == nothing
        @test x1 == nothing
    end

    # match struct with mix of by-value and by-field name
    let x1 = nothing
        @test (@match Foo(1,2) begin
               Foo(0,2) => nothing
               Foo(x=x1) => x1
        end) == 1
    end

    # match multiple struct fields by name
    let x1 = nothing, y1 = nothing
        @test (@match Foo(1,2) begin
               Foo(x=x1,y=y1) => (x1,y1)
        end) == (1,2)
    end

    # match struct field by name redundantly
    let x1 = nothing, x2 = nothing
        @test_throws assertion_error @eval (@match Foo(1,2) begin
               Foo(x=x1,x=x2) => (x1,x2)
        end)
    end

    # variables in patterns are local, and can match multiple positions
    let z = 0
        @test z == 0
        @test (@match Foo(1,1) begin
               Foo(x=z, y=z) => z # inner z matches both x and y
               end) == 1
        @test z == 0 # no change to outer z
    end

    # variable in a pattern can match multiple positions
    @test_throws MatchFailure @eval (@match Foo(1,2) begin
                                     Foo(x=x1, y=x1) => true
                                     end)
end

@testset "non-struct Matches" begin
    # throw MatchFailure if no matches
    @test_throws MatchFailure @match :this begin
        :that => :ok
    end

    # match against symbols
    @test (@match :this begin
        :this => :ok
    end) == :ok

    # treat macros as constants
    @test (@match v"1.2.0" begin
      v"1.2.0" => :ok
    end) == :ok

    # QuoteNodes
    @test (@match :(:x) begin
      :(:x) => :ok
    end) == :ok
    @test (@match :(:x+:y) begin
      :(:x + :y) => :ok
    end) == :ok
end

@testset "logical expressions with branches" begin
    # disjunction
    @test (@match (1,(2,3)) begin
      (1, (x,:nope) || (2,x)) => x
    end) == 3

    # disjunction and repeated variables
    @test (@match (1,(2,3), 3) begin
      (1, (x,:nope) || (2,x), x) => x
    end) == 3
    @test (@match (1,(2,3), 4) begin
      (1, (x,:nope) || (2,x), x) => x
      _ => :ok
    end) == :ok
    @test (@match (3,(2,3), 3) begin
      (x, (x,:nope) || (2,x), 3) => x
    end) == 3
    @test (@match (1,(2,3), 3) begin
      (x, (x,:nope) || (2,x), 3) => x
      _ => :ok
    end) == :ok
    @test (@match (3,(2,3), 3) begin
      (x, (x,:nope) || (2,x), x) => x
    end) == 3
    @test (@match (3,(2,3), 1) begin
      (x, (x,:nope) || (2,x), x) => x
      _ => :ok
    end) == :ok

    # conjunction
    @test (@match (1,(2,3)) begin
        (1, a && (2,b)) => (a,b)
    end) == ((2,3),3)
    @test_throws MatchFailure (@match (1,(2,3)) begin
        (1, a && (1,b)) => (a,b)
    end) == ((2,3),3)

    # only vars that exist in all branches can be accessed
    @test_throws UndefVarError @eval @match (1,(2,3)) begin
      (1, (x,:nope) || (2,y)) => y
    end
end

@testset "Splats" begin
    # splats
    test0(x) = @match x begin
        [a] => [a]
        [a,b,c...] => [a,b,c]
        (a,) => (a,)
        (a...,b,c,d) => (a,b,c,d)
        (a,b...,c) => (a,b,c)
    end
    @test test0([1]) == [1]
    @test test0([1,2]) == [1,2,[]]
    @test test0([1,2,3]) == [1,2,[3]]
    @test test0([1,2,3,4]) == [1,2,[3,4]]
    @test test0((1,)) == (1,)
    @test test0((1,2)) == (1, (), 2)
    @test test0((1,2,3)) == ((), 1, 2, 3)
    @test test0((1,2,3,4)) == ((1,), 2, 3, 4)
    @test test0((1,2,3,4,5)) == ((1,2), 3, 4, 5)

    # no splats allowed in structs (would be nice, but need to implement getfield(struct, range))
    @test_throws assertion_error @eval @match foo begin
        Foo(x...) => :nope
    end

    # at most one splat in tuples/arrays
    @test_throws assertion_error @eval @match [1,2,3] begin
        [a...,b,c...] => :nope
    end
    @test_throws assertion_error @eval @match [1,2,3] begin
        (a...,b,c...) => :nope
    end

    # inference for splats
    infer1(x) = @match x begin
        (a, b..., c) => a
    end
    @test @code_typed(infer1((:ok,2,3,4)))[2] == Symbol
    infer2(x) = @match x begin
        (a, b..., c) => c
    end

    if VERSION >= v"0.7.0-DEV"
        @test @code_typed(infer2((1,2,3,:ok)))[2] == Symbol
    else
        # can't infer x[4-0] - fixed in Julia 0.7
        @test_broken @code_typed(infer2((1,2,3,:ok)))[2] == Symbol
    end
end

@testset "Inference in branches" begin
    # inference in branches
    infer3(foo) = @match foo begin
        Foo(_,y::Symbol) => y
        Foo(x::Symbol,_) => x
    end
    @test @code_typed(infer3(Foo(1,2)))[2] == Symbol
    infer4(foo) = @match foo begin
        Foo(x,y::Symbol) => y
        Foo(x::Symbol,y) => x
    end
    # TODO should we `let` in branches rather than exporting variables?
    @test_broken @code_typed(infer4(Foo(1,2)))[2] == Symbol
end

@testset "Nested Guards" begin
    # nested guards can use earlier bindings
    @test (@match [1,2] begin
      [x, y where y > x] => (x,y)
    end) == (1,2)
    @test_throws MatchFailure @match [2,1] begin
      [x, y where y > x] => (x,y)
    end

    # nested guards can't use later bindings
    @test_throws UndefVarError @eval @match [2,1] begin
      [x where y > x, y ] => (x,y)
    end
end

@testset "structs matching all fields" begin
    # detect incorrect numbers of fields
    @test_throws ErrorException @match Foo(x) = Foo(1,2)
    @test_throws ErrorException @match Foo(x,y,z) = Foo(1,2)

    # ...even if the pattern is not reached
    @test_throws ErrorException (@match Foo(1,2) begin
        Foo(x,y) => :ok
        Foo(x) => :nope
    end)
end

@testset "Miscellanea" begin
    # match against fiddly symbols (https://github.com/kmsquire/Match.jl/issues/32)
    if VERSION >= v"0.7.0-DEV"
        @test (@match :(@when a < b) begin
               Expr(_, [Symbol("@when"), _, _]) => :ok
               Expr(_, [other, _, _]) => other
               end) == :ok
    else
        @test (@match :(@when a < b) begin
               Expr(_, [Symbol("@when"), _], _) => :ok
               Expr(_, [other, _], _) => other
               end) == :ok
    end

    # test repeated variables (https://github.com/kmsquire/Match.jl/issues/27)
    @test (@match (x,x) = (1,1)) == (1,1)
    @test_throws MatchFailure @match (x,x) = (1,2)

    # match against single tuples (https://github.com/kmsquire/Match.jl/issues/43)
    @test (@match (:x,) begin
      (:x,) => :ok
    end) == :ok

    # match against empty structs (https://github.com/kmsquire/Match.jl/issues/43)
    @eval struct True end
    e = (True(), 1)
    @test (@match e begin
        (True(), x) => x
    end) == 1

    # symbols are not interpreted as variables (https://github.com/kmsquire/Match.jl/issues/45)
    let x = 42
        @test (@match (:x,) begin
          (:x,) => x
        end) == 42
    end

    # allow & and | for conjunction/disjunction (https://github.com/RelationalAI-oss/Rematch.jl/issues/1)
    @test (@match (1,(2,3)) begin
      (1, (x,:nope) | (2,x)) => x
    end) == 3
    @test (@match (1,(2,3)) begin
        (1, a & (2,b)) => (a,b)
    end) == ((2,3),3)

    # don't treat infix operators like structs
    @test_throws assertion_error @eval @match a + b = x
end

@testset "Interpolated Values" begin
    # match against interpolated values
    let outer = 2, b = nothing, c = nothing
        @test (@match [1, $outer] = [1,2]) == [1,2]
        @test (@match (1, $outer, b..., c) = (1,2,3,4,5)) == (1,2,3,4,5)
        @test b == (3,4)
        @test c == 5
    end
    test_interp_pattern = let a=1, b=2, c=3,
                              arr=[10,20,30], tup=(100,200,300)
        _t(x) = @match x begin
            # scalars
            [$a,$b,$c,out] => out
            [fronts..., $a,$b,$c, back] => [fronts...,back]
            # arrays & tuples
            [fronts..., $arr, back] => [fronts...,back]
            [fronts..., $tup, back] => [fronts...,back]
            # complex expressions
            [$(a+b+c), out] => out
            # splatting existing values
            [fronts..., $(arr...), back] => [fronts...,back]
        end
    end
    # scalars
    @test test_interp_pattern([1,2,3,4]) == 4
    @test test_interp_pattern([4,3,2,1, 1,2,3, 4]) == [4,3,2,1,4]
    # arrays & tuples
    @test test_interp_pattern([0,1, [10,20,30], 2]) == [0,1,2]
    @test test_interp_pattern([0,1, (100,200,300), 2]) == [0,1,2]
    # complex expressions
    @test test_interp_pattern([6,1]) == 1
    # TODO: splatting existing values into pattern isn't suported yet.
    @test_broken test_interp_pattern([0,1, 10,20,30, 2]) == [0,1,2]
end

# --- tests from Match.jl ---

@testset "Tests imported from Match.jl" begin

    # Type matching
    test1(item) = @match item begin
        n::Int                       => "Integers are awesome!"
        str::AbstractString          => "Strings are the best"
        m::Dict{Int,AbstractString}  => "Ints for Strings?"
        d::Dict                      => "A Dict! Looking up a word?"
        _                            => "Something unexpected"
    end

    d = Dict{Int,AbstractString}(1 => "a", 2 => "b")

    @test test1(66)     == "Integers are awesome!"
    @test test1("abc")  == "Strings are the best"
    @test test1(d)      == "Ints for Strings?"
    @test test1(Dict()) == "A Dict! Looking up a word?"
    @test test1(2.0)    == "Something unexpected"

    @eval struct Address
        street::AbstractString
        city::AbstractString
        zip::AbstractString
    end

    @eval struct Person
        firstname::AbstractString
        lastname::AbstractString
        address::Address
    end

    test2(person) = @match person begin
        Person("Julia", lastname,  _) => "Found Julia $lastname"
        Person(firstname, "Julia", _) => "$firstname Julia was here!"
        Person(firstname, lastname, Address(_, "Cambridge", zip)) => "$firstname $lastname lives in zip $zip"
        _::Person  => "Unknown person!"
    end

    @test test2(Person("Julia", "Robinson", Address("450 Serra Mall", "Stanford", "94305")))         == "Found Julia Robinson"
    @test test2(Person("Gaston", "Julia",   Address("1 rue Victor Cousin", "Paris", "75005")))       == "Gaston Julia was here!"
    @test test2(Person("Edwin", "Aldrin",   Address("350 Memorial Dr", "Cambridge", "02139")))       == "Edwin Aldrin lives in zip 02139"
    @test test2(Person("Linus", "Pauling",  Address("1200 E California Blvd", "Pasadena", "91125"))) == "Unknown person!"  # Really?

    @eval abstract type Term end

    @eval struct Var <: Term
        name::AbstractString
    end

    @eval struct Fun <: Term
        arg::AbstractString
        body::Term
    end

    @eval struct App <: Term
        f::Term
        v::Term
    end

    function test_show(io::IO, term::Term)
        @match term begin
           Var(n)    => print(io, n)
           Fun(x, b) => begin
                            print(io, "^$x.")
                            test_show(io, b)
                        end
           App(f, v) => begin
                            print(io, "(")
                            test_show(io, f)
                            print(io, " ")
                            test_show(io, v)
                            print(io, ")")
                        end
        end
    end

    function is_identity_fun(term::Term)
       @match term begin
         Fun(x, Var(y)) where x == y => true
         _ => false
       end
    end

    id = Fun("x", Var("x"))
    t = Fun("x", Fun("y", App(Var("x"), Var("y"))))

    let io = IOBuffer()
        test_show(io, id)
        @test String(take!(io)) == "^x.x"
        test_show(io, t)
        @test String(take!(io)) == "^x.^y.(x y)"
        @test is_identity_fun(id)
        @test !is_identity_fun(t)
    end

    myisodd(x::Int) = @match(x, i => i % 2 == 1)
    @test filter(myisodd, 1:10) == filter(isodd, 1:10) == [1, 3, 5, 7, 9]

    function parse_arg(arg::AbstractString, value::Any=nothing)
       @match (arg, value) begin
          ("-l",              lang) where lang != nothing => "Language set to $lang"
          ("-o" || "--optim", n::Int) where 0 < n <= 5 => "Optimization level set to $n"
          ("-o" || "--optim", n::Int)                         => "Illegal optimization level $(n)!"
          ("-h" || "--help",  nothing)                        => "Help!"
          bad                                                 => "Unknown argument: $bad"
       end
    end

    @test parse_arg("-l", "eng")  == "Language set to eng"
    @test parse_arg("-l")         == "Unknown argument: (\"-l\", nothing)"
    @test parse_arg("-o", 4)      == "Optimization level set to 4"
    @test parse_arg("--optim", 5) == "Optimization level set to 5"
    @test parse_arg("-o", 0)      == "Illegal optimization level 0!"
    @test parse_arg("-o", 1.0)    == "Unknown argument: (\"-o\", 1.0)"

    @test parse_arg("-h") == parse_arg("--help") == "Help!"

    function fizzbuzz(range::AbstractRange)
        io = IOBuffer()
        for n in range
            @match (n % 3, n % 5) begin
                (0, 0) => print(io, "fizzbuzz ")
                (0, _) => print(io, "fizz ")
                (_, 0) => print(io, "buzz ")
                (_, _) => print(io, n, ' ')
            end
        end
        String(take!(io))
    end

    @test fizzbuzz(1:15) == "1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz "

    @eval abstract type RBTree end

    @eval struct Leaf <: RBTree
    end

    @eval struct Red <: RBTree
        value
        left::RBTree
        right::RBTree
    end

    @eval struct Black <: RBTree
        value
        left::RBTree
        right::RBTree
    end

    function balance(tree::RBTree)
        @match tree begin
            (Black(z, Red(y, Red(x, a, b), c), d)
             || Black(z, Red(x, a, Red(y, b, c)), d)
             || Black(x, a, Red(z, Red(y, b, c), d))
             || Black(x, a, Red(y, b, Red(z, c, d)))) => Red(y, Black(x, a, b),
                                                             Black(z, c, d))
            _ => tree
        end
    end

    @test balance(Black(1, Red(2, Red(3, Leaf(), Leaf()), Leaf()), Leaf())) ==
                Red(2, Black(3, Leaf(), Leaf()),
                    Black(1, Leaf(), Leaf()))


    function num_match(n)
        @match n begin
            0      => "zero"
            1 || 2 => "one or two"
            # 3:10   => "three to ten"
            _      => "something else"
        end
    end

    @test num_match(0) == "zero"
    @test num_match(2) == "one or two"
    # @test num_match(4) == "three to ten"
    @test num_match(12) == "something else"
    @test num_match("hi") == "something else"
    @test num_match('c') == "something else"

    # Interpolation of matches in quoted expressions
    test_interp(item) = @match item begin
        [a, b] => :($a + $b)
    end
    @test test_interp([1, 2]) == :(1 + 2)
end
