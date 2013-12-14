module Lazy

############
# Utilities
############

export @listable, @>, @>>

macro listable(f)
  if typeof(f) == Expr && f.head == :tuple
    return Expr(:block, [:(@listable $f) for f in f.args]...)
  end
  f = esc(f)
  :($f(ls::List...) = map($f, ls...))
end

# Threading macros

macro >(x, expr, exprs...)
  if typeof(expr) == Expr && expr.head == :tuple
    return Expr(:macrocall, symbol("@>"), esc(x), map(esc,expr.args)..., map(esc,exprs)...)

  elseif typeof(expr) == Symbol
    call = esc(Expr(:call, expr, x))

  elseif typeof(expr) == Expr && expr.head == :call
    call = esc(Expr(:call, expr.args[1], x, expr.args[2:]...))

  elseif typeof(expr) == Expr && expr.head == :->
    call = esc(Expr(:call, expr, x))

  else
    error("Unsupported expression $expr in @>")
  end
  isempty(exprs) ? call : :(@> $call $(map(esc,exprs)...))
end

macro >>(x, expr, exprs...)
  if typeof(expr) == Expr && expr.head == :tuple
    return Expr(:macrocall, symbol("@>>"), esc(x), map(esc,expr.args)..., map(esc,exprs)...)

  elseif typeof(expr) == Symbol
    call = esc(Expr(:call, expr, x))

  elseif typeof(expr) == Expr && expr.head == :call
    call = esc(Expr(:call, expr.args..., x))

  elseif typeof(expr) == Expr && expr.head == :->
      call = esc(Expr(:call, expr, x))
  else
    error("Unsupported expression $expr in @>>")
  end
  isempty(exprs) ? call : :(@>> $call $(map(esc,exprs)...))
end

########
# Types
########

import Base: isempty, first

export List, EmptyList, LinkedList, LazyList,
       list, @lazy,
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

# first(::EmptyList) = nothing
# rest(l::EmptyList) = l
first(::EmptyList) = throw(BoundsError())
rest(::EmptyList) = throw(BoundsError())
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

# ------------
# Construction
# ------------

export seq, constantly, repeatedly, cycle, iterate, range,
       concat

seq(xs::List) = xs
seq(xs::Array) = isempty(xs) ? list() : xs[1]:seq(xs[2:])

seq(itr) = seq(itr, start(itr))
seq(itr, state) =
  @lazy if done(itr, state)
    []
  else
    x, state = next(itr, state)
    prepend(x, seq(itr, state))
  end

constantly(x) = @lazy x:constantly(x)
constantly(n, x) = @>> constantly(x) take(n)

cycle(xs) = @lazy xs * cycle(xs)

repeatedly(f) = @lazy f():repeatedly(f)
repeatedly(n, f) = @>> repeatedly(f) take(n)

iterate(f, v) = @lazy v:iterate(f, f(v))

range(x, y, step=1) =
  @lazy x <= y ? (x:range(x+step, y, step)) : []
range(x=1) = # Optimisation for y=Inf
  @lazy prepend(x, range(x+1))

concat(xs::List, ys::List) =
  @lazy isempty(xs) ? ys : prepend(first(xs), concat(rest(xs), ys))

*(xs::List, ys::List) = concat(xs, ys)
*(xs, ys::List) = concat(seq(xs), ys)
*(xs::List, ys) = concat(xs, seq(ys))

# ------------
# Manipulation
# ------------

import Base: length, map, reduce, filter, reverse, colon

export riffle, interpose, take, drop, take_last, drop_last, take_nth, take_while, drop_while,
       lazymap, reductions, remove, dorun, foreach, distinct,
       group_by, partition, partition_by, split_at, split_by

riffle(ls...) = riffle(map(seq, ls)...)
riffle(ls::List...) =
  @lazy any(isempty, ls) ? [] : seq(map(first, ls)) * riffle(map(rest, ls)...)

interpose(xs, args...) = interpose(seq(xs), args...)
interpose(xs::List, y, n = 1) =
  @lazy isempty(xs) ? [] :
    take(n, xs) * (isempty(drop(n, xs)) ? [] : prepend(y, interpose(drop(n, xs), y, n)))

length(l::List) = isempty(l) ? 0 : 1 + length(rest(l))

Base.endof(l::List) = error("Cant use `end` with List.")

take(n::Integer, l::List) =
  @lazy n <= 0 || isempty(l) ? [] : prepend(first(l), take(n-1, rest(l)))

drop(n::Integer, l::List) =
  @lazy n <= 0 ? l : drop(n-1, rest(l))

take_last(n::Integer, l::List) =
  @lazy isempty(drop(n, l)) ? l : take_last(n, rest(l))

drop_last(n::Integer, l::List) =
  map((x,_)->x, l, drop(n,l))

take_nth(n::Integer, l::List) =
  @lazy isempty(l) ? [] : first(drop(n-1,l)):take_nth(n, drop(n, l))

for f in [:take :drop :take_last :drop_last :take_nth]
  @eval $f(l::List, n::Integer) = $f(n, l)
end

take_while(pred::Function, l::List) =
  @lazy isempty(l) || !pred(first(l)) ? [] : first(l):take_while(pred, rest(l))

drop_while(pred::Function, l::List) =
  @lazy isempty(l) || !pred(first(l)) ? l : drop_while(pred, rest(l))

mapply(f::Union(Function, DataType), ls...) =
  @lazy any(isempty, ls) ? [] : prepend(f(map(first, ls)...), mapply(f, map(rest, ls)...))

# Resolves amibguity error
map(f::Function, ls::List...) = mapply(f, ls...)
map(f::DataType, ls::List...) = mapply(f, ls...)

lazymap(f::Union(Function, DataType), ls...) = map(f, map(seq, ls)...)

reduce(f::Function, v, xs::List) =
  isempty(xs) ? v : reduce(f, f(v, first(xs)), rest(xs))

reduce(f::Function, xs::List) =
  isempty(xs) ? f() : reduce(f, first(xs), rest(xs))

reductions(f::Function, v, xs::List) =
  @lazy isempty(xs) ? [] : v:reductions(f, f(v, first(xs)), rest(xs))

reductions(f::Function, xs::List) =
  @lazy isempty(xs) ? [] : reductions(f, first(xs), rest(xs))

filter(f::Function, xs::List) =
  @lazy if isempty(xs)
    []
  elseif f(first(xs))
    first(xs):filter(f, rest(xs))
  else
    filter(f, rest(xs))
  end

remove(f::Function, xs::List) = filter(x->!f(x), xs)

reverse(xs::List) = reduce((xs, x)->x:xs, list(), xs)

distinct(xs::List) = distinct(xs, Set())
distinct(xs::List, seen::Set) =
  @lazy isempty(xs) ? [] :
    first(xs) in seen ?
      distinct(rest(xs), seen) :
      first(xs):distinct(rest(xs), push!(seen, first(xs)))

function group_by(f, xs::List)
  groups = Dict()
  for x in xs
    k = f(x)
    groups[k] = x:get(groups, k, list())
  end
  return groups
end

partition(n, xs::List; step = n, pad = nothing) =
  @lazy isempty(xs) ? [] :
    let l = take(n, xs), len = length(l)
      if len < n
        pad == nothing ? [] : list(l * take(n-len, pad))
      else
        l:partition(n, drop(step, xs); step = n, pad = pad)
      end
    end

partition_by(f, xs::List) =
  @lazy isempty(xs) ? [] :
    let x = first(xs), v = f(x), run = take_while(x->f(x)==v, rest(xs))
      prepend(x,run):partition_by(f, @lazy drop(length(run)+1, xs))
    end

split_at(n, xs::List) = (take(n, xs), drop(n, xs))

split_by(p, xs::List) =
  let run = take_while(p, xs)
    run, @lazy drop(length(run), xs)
  end

# ----------
# Predicates    
# ----------

import Base: any, all

# any(f::Function, xs::List) = @>> xs map(f) any
# any(xs::List) = isempty(xs) ? false : first(xs) || any(rest(xs))

# all(f::Function, xs::List) = @>> xs map(f) all
# all(xs::List) = isempty(xs) ? true : first(xs) && rest(xs)

# -----
# Eager
# -----

export dorun, doall, foreach

dorun(xs::List) = isempty(xs) ? nothing : dorun(rest(xs))
doall(xs::List) = (dorun(xs); xs)

foreach(f, ls::List...) = map(f, ls...) |> dorun

# -------
# Interop
# -------

import Base: getindex, setindex!, start, next, done

getindex(l::List, i::Int) = i <= 1 ? first(l) : rest(l)[i-1]
getindex(l::List, r::Range1) = take(r.len, drop(r.start - 1, l))

setindex!(xs::LinkedList, v, i::Integer) = i <= 1 ? xs.first = v : (rest(xs)[i-1] = v)
setindex!(xs::LazyList, v, i::Integer) = i <= 1 ? realise(xs)[1] = v : (rest(xs)[i-1] = v)

# Iteration over a list holds on to the head
start(xs::List) = xs
done(::List, xs::List) = isempty(xs)
next(::List, xs::List) = first(xs), rest(xs)

###########
# Printing
###########

function Base.show(io::IO, xs::List)
  # print(io, "(")
  foreach(x->print(io,x), ["("] * interpose(xs, " ") * [")"])
  # print(io, ")")
end

function Base.display(d::TextDisplay, xs::List)
  if isempty(xs) println(d.io, "List()"); return; end
  print(d.io, "List:")
  for x in xs
    println(d.io)
    show(d.io, x)
  end
end

# Some example lists

@listable +, -, Base.exp, Base.log, Base.sin, Base.cos, Base.tan

fibs = @lazy 0:big(1):(fibs + fibs[2:])

isprime(n) =
  @>>(primes,
      take_while(x -> x<=sqrt(n)),
      map(x-> n % x == 0),
      not_any)

primes = filter(isprime, range(2))

end
