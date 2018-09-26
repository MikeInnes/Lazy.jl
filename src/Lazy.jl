module Lazy

############
# Utilities
############

using Compat

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
  :($f(l1::List, ls::List...) = map($f, l1, ls...))
end

########
# Types
########

import Base: isempty, first

export List, list, @lazy, prepend, tail

abstract type List end

mutable struct EmptyList <: List
end

mutable struct LinkedList <: List
  head
  tail::List
end

mutable struct LazyList <: List
  list::List
  realised::Bool
  f::Function

  LazyList(f) = new(EmptyList(), false, f)
end

# Empty List

first(::EmptyList) = nothing
tail(l::EmptyList) = l
isempty(::EmptyList) = true

# Lists

prepend(x, l::List) = LinkedList(x, l)
(::Colon)(x, xs::List) = prepend(x, xs)
(::Colon)(x::List, xs::List) = prepend(x, xs) # special case: prepend list
(::Colon)(x,y,xs::List) = x:prepend(y,xs)

list() = EmptyList()
list(x, xs...) = x:list(xs...)

first(l::LinkedList) = l.head
tail(l::LinkedList) = l.tail

isempty(::LinkedList) = false

# Lazy Lists

function realise!(xs::LazyList)
  xs.realised && return xs.list
  xs.realised = true
  xs.list = xs.f()
  return xs.list
end

function realise(xs::LazyList)
  realise!(xs)
  # Unroll in a loop to avoid overflow
  while isa(xs.list, LazyList)
    xs.list = realise!(xs.list)
  end
  return xs.list
end

macro lazy(code)
  :(LazyList(() -> seq($(esc(code)))))
end

for f in [:first :isempty]
  @eval $f(l::LazyList) = $f(realise(l))
end

tail(l::LazyList) = @lazy tail(realise(l))

########
# Usage
########

include("liblazy.jl")
include("collections.jl")

# -----
# Eager
# -----

# Depending upon Julia and Compat versions, foreach may be defined as
# a core language generic function. If so, extend rather than define
# it.

if isdefined(Base, :foreach) && isa(foreach, Function)
    import Base.foreach
end

export dorun, doall, foreach

@rec dorun(xs::List) = isempty(xs) ? nothing : dorun(tail(xs))
doall(xs::List) = (dorun(xs); xs)

foreach(f, ls::List...) = map(f, ls...) |> dorun

# -------
# Interop
# -------

import Base: getindex, setindex!

getindex(l::List, i::Int) = i <= 1 ? first(l) : tail(l)[i-1]
getindex(l::List, r::UnitRange) = take(r.len, drop(r.start - 1, l))

setindex!(xs::LinkedList, v, i::Integer) = i <= 1 ? xs.first = v : (tail(xs)[i-1] = v)
setindex!(xs::LazyList, v, i::Integer) = i <= 1 ? realise(xs)[1] = v : (tail(xs)[i-1] = v)

# Iteration over a list holds on to the head
function Base.iterate(L::List, xs::List=L)
  isempty(xs) && return nothing
  first(xs), tail(xs)
end

###########
# Printing
###########

Base.show(io::IO, xs::List) =
  foreach(x->print(io,x), ["("] * interpose(xs, " ") * [")"])

function show(io::IO, ::MIME"text/plain", xs::List)
  isempty(xs) && return println(io, "List()")

  print(io, "List:")
  for x in xs
    print(io, "\n  ")
    show(io, x)
  end
end

# Some example lists

@listable +, -, Base.exp, Base.log, Base.sin, Base.cos, Base.tan

const fibs = @lazy 0:big(1):(fibs + tail(fibs))

isprime(n) =
  @>> primes begin
    takewhile(x -> x<=sqrt(n))
    map(x -> n % x == 0)
    any; !
  end

const primes = filter(isprime, range(2))

end
