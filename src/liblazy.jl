# ------------
# Construction
# ------------

export seq, constantly, repeatedly, cycle, iterate, range,
       concat

seq(xs::List) = xs
seq(xs::Array) = isempty(xs) ? list() : xs[1]:seq(xs[2:end])

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
*(xs::BitArray, ys::List) = concat(seq(xs), ys)
*(xs::AbstractArray, ys::List) = concat(seq(xs), ys)
*(xs::List, ys::AbstractArray) = concat(xs, seq(ys))

# ------------
# Manipulation
# ------------

import Base: length, map, reduce, filter, reverse

export riffle, interpose, take, drop, take_last, drop_last, take_nth, takewhile, drop_while,
       lazymap, reductions, remove, dorun, foreach, distinct,
       group_by, partition, partition_by, splitat, splitby,
       walk, prewalk, postwalk, flatten

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

Base.last(l::List) = @>> l take_last(1) first

drop_last(n::Integer, l::List) =
  map((x,_)->x, l, drop(n,l))

take_nth(n::Integer, l::List) =
  @lazy isempty(l) ? [] : first(drop(n-1,l)):take_nth(n, drop(n, l))

for f in [:take :drop :take_last :drop_last :take_nth]
  @eval $f(l::List, n::Integer) = $f(n, l)
end

takewhile(pred::Function, l::List) =
  @lazy isempty(l) || !pred(first(l)) ? [] : first(l):takewhile(pred, rest(l))

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
        l:partition(n, drop(step, xs); step = step, pad = pad)
      end
    end

partition_by(f, xs::List) =
  @lazy isempty(xs) ? [] :
    let x = first(xs), v = f(x), run = takewhile(x->f(x)==v, rest(xs))
      prepend(x,run):partition_by(f, @lazy drop(length(run)+1, xs))
    end

splitat(n, xs::List) = (take(n, xs), drop(n, xs))

splitby(p::Function, xs::List) = takewhile(p, xs), drop_while(p, xs)

walk(inner, outer, xs::List) = @>> xs map(inner) outer
walk(inner, outer, x) = outer(x)

prewalk(f, xs)  = walk(x -> prewalk(f, x), identity, f(xs))
postwalk(f, xs) = walk(x -> postwalk(f, x), f, xs)

flatten(x) = x
flatten(xs::List) = reduce((xs, x) -> isa(x, List) ? xs*x : xs*list(x), list(), map(flatten, xs))

# ----------
# Predicates
# ----------

import Base: any, all

==(xs::List, ys::List) =
  isempty(xs) == isempty(ys) &&
    (isempty(xs) || first(xs) == first(ys) && rest(xs) == rest(ys))

any(f::Function, xs::List) = @>> xs map(f) any
any(xs::List) = isempty(xs) ? false : first(xs) || any(rest(xs))

all(f::Function, xs::List) = @>> xs map(f) all
all(xs::List) = isempty(xs) ? true : first(xs) && all(rest(xs))
