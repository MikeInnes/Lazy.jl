VERSION > v"0.4-" && __precompile__()

module Lazy

############
# Utilities
############

include("macros.jl")
include("dynamic.jl")
include("tail.jl")

export @listable

import Base: *, ==, +, -

macro listable(f)
  if typeof(f) == Expr && f.head == :tuple
    return Expr(:block, [:(@listable $f) for f in f.args]...)
  end
  f = esc(f)
  :($f(ls::List...) = map($f, ls...))
end

########
# Types
########

import Base: isempty, first, colon

export List, list, @lazy,
       prepend, rest

abstract List

type EmptyList <: List
end

type LinkedList <: List
  first
  rest::List
end

type LazyList <: List
  list::List
  realised::Bool
  f::Function

  LazyList(f) = new(EmptyList(), false, f)
end

# Empty List

first(::EmptyList) = nothing
rest(l::EmptyList) = l
isempty(::EmptyList) = true

# Lists

prepend(x, l::List) = LinkedList(x, l)
colon(x, xs::List) = prepend(x, xs)
colon(x,y,xs::List) = x:prepend(y,xs)

list() = EmptyList()
list(x, xs...) = x:list(xs...)

first(l::LinkedList) = l.first
rest(l::LinkedList) = l.rest

isempty(::LinkedList) = false

# Lazy Lists

realise(xs::LazyList) =
  xs.realised? xs.list : (xs.realised = true; xs.list = xs.f())

for f in [:first :rest :isempty]
  @eval $f(l::LazyList) = $f(realise(l))
end

macro lazy(code)
  :(LazyList(() -> seq($(esc(code)))))
end

########
# Usage
########

include("liblazy.jl")
include("collections.jl")

# -----
# Eager
# -----

export dorun, doall, foreach

@rec dorun(xs::List) = isempty(xs) ? nothing : dorun(rest(xs))
doall(xs::List) = (dorun(xs); xs)

foreach(f, ls::List...) = map(f, ls...) |> dorun

# -------
# Interop
# -------

import Base: getindex, setindex!, start, next, done

getindex(l::List, i::Int) = i <= 1 ? first(l) : rest(l)[i-1]
getindex(l::List, r::UnitRange) = take(r.len, drop(r.start - 1, l))

setindex!(xs::LinkedList, v, i::Integer) = i <= 1 ? xs.first = v : (rest(xs)[i-1] = v)
setindex!(xs::LazyList, v, i::Integer) = i <= 1 ? realise(xs)[1] = v : (rest(xs)[i-1] = v)

# Iteration over a list holds on to the head
start(xs::List) = xs
done(::List, xs::List) = isempty(xs)
next(::List, xs::List) = first(xs), rest(xs)

###########
# Printing
###########

Base.show(io::IO, xs::List) =
  foreach(x->print(io,x), ["("] * interpose(xs, " ") * [")"])

function Base.writemime(io::IO, ::MIME"text/plain", xs::List)
  isempty(xs) && return println(io, "List()")

  print(io, "List:")
  for x in xs
    print(io, "\n  ")
    show(io, x)
  end
end

# Some example lists

@listable +, -, Base.exp, Base.log, Base.sin, Base.cos, Base.tan

const fibs = @lazy 0:big(1):(fibs + drop(1, fibs))

isprime(n) =
  @>> primes begin
    takewhile(x -> x<=sqrt(n))
    map(x -> n % x == 0)
    any; !
  end

const primes = filter(isprime, range(2))

end
