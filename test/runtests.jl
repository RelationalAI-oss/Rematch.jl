module TestRematch

using Rematch
import Rematch: MatchFailure
using Base.Test

# --- basic tests ---

struct Foo
    x
    y
end

# test basic match
x = nothing
@test (@match Foo(x,2) = Foo(1,2)) == Foo(1,2)
@test x == 1

# variables not bound if match fails
x = nothing
@test_throws MatchFailure @match Foo(x, 3) = Foo(1,2)
@test x == nothing

# variables not bound if guard fails
x = nothing
@test_throws MatchFailure @match Foo(1,2) begin
  Foo(x, 2) where x != 1 => :ok
end
@test x == nothing

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
@test_throws AssertionError @eval @match foo begin
    Foo(x...) => :nope
end

# at most one splat in tuples/arrays
@test_throws AssertionError @eval @match [1,2,3] begin
    [a...,b,c...] => :nope
end
@test_throws AssertionError @eval @match [1,2,3] begin
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
# can't infer x[4-0] - fixed in Julia 0.7
@test_broken @code_typed(infer2((1,2,3,:ok)))[2] == Symbol

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

# detect incorrect numbers of fields
@test_throws AssertionError @match Foo(x) = Foo(1,2)
@test_throws AssertionError @match Foo(x,y,z) = Foo(1,2)

# ...even if the pattern is not reached
@test_throws AssertionError (@match Foo(1,2) begin
    Foo(x,y) => :ok
    Foo(x) => :nope
end)

# match against fiddly symbols (https://github.com/kmsquire/Match.jl/issues/32)
@test (@match :(@when a < b) begin
  Expr(_, [Symbol("@when"), _], _) => :ok
  Expr(_, [other, _], _) => other
end) == :ok

# test repeated variables (https://github.com/kmsquire/Match.jl/issues/27)
@test (@match (x,x) = (1,1)) == (1,1)
@test_throws MatchFailure @match (x,x) = (1,2)

# match against single tuples (https://github.com/kmsquire/Match.jl/issues/43)
@test (@match (:x,) begin
  (:x,) => :ok
end) == :ok

# match against empty structs (https://github.com/kmsquire/Match.jl/issues/43)
struct True end
e = (True(), 1)
@test (@match e begin
    (True(), x) => x
end) == 1

# symbols are not interpreted as variables (https://github.com/kmsquire/Match.jl/issues/45)
x = 42
@test (@match (:x,) begin
  (:x,) => x
end) == 42

# --- tests from Match.jl ---

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

struct Address
    street::AbstractString
    city::AbstractString
    zip::AbstractString
end

struct Person
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

abstract type Term end

struct Var <: Term
    name::AbstractString
end

struct Fun <: Term
    arg::AbstractString
    body::Term
end

struct App <: Term
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

function fizzbuzz(range::Range)
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

abstract type RBTree end

struct Leaf <: RBTree
end

struct Red <: RBTree
    value
    left::RBTree
    right::RBTree
end

struct Black <: RBTree
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
        tree => tree
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
