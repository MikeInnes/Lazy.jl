# ------------
# Construction
# ------------

export seq, constantly, repeatedly, iterate, concat

seq(xs::List) = xs
seq(xs::Array) = isempty(xs) ? list() : xs[1]:seq(xs[2:end])

seq(xs::Tuple) = seq(collect(xs))

seq(itr) = seq(itr, start(itr))
seq(itr, state) =
  @lazy done(itr, state) ? [] :
    begin x, state = next(itr, state)
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
  @lazy x:range(x+1)

concat(xs::List, ys::List) =
  @lazy isempty(xs) ? ys : (first(xs):concat(tail(xs), ys))

*(xs::List, ys::List) = concat(xs, ys)
*(xs::BitArray, ys::List) = concat(seq(xs), ys)
*(xs::AbstractArray, ys::List) = concat(seq(xs), ys)
*(xs::List, ys::AbstractArray) = concat(xs, seq(ys))

# ------------
# Manipulation
# ------------

import Base: length, map, reduce, filter, reverse, Predicate

if VERSION >= v"0.4.0"
    import Base: drop, take
end

export riffle, interpose, take, drop, takelast, droplast, takenth, takewhile, dropwhile,
       lazymap, reductions, remove, dorun, foreach, distinct,
       groupby, partition, partitionby, splitat, splitby, flatten

riffle(ls...) = riffle(map(seq, ls)...)
riffle(ls::List...) =
  @lazy any(isempty, ls) ? [] :
    seq(map(first, ls)) * riffle(map(tail, ls)...)

interpose(xs, args...) = interpose(seq(xs), args...)
interpose(xs::List, y, n = 1) =
  @lazy isempty(xs) ? [] :
       take(n, xs) * (isempty(drop(n, xs)) ? [] :
                        prepend(y, interpose(drop(n, xs), y, n)))

length(l::List) = isempty(l) ? 0 : 1 + length(tail(l))

Base.endof(l::List) = error("Cant use `end` with List.")

take(n::Integer, l::List) =
  @lazy n <= 0 || isempty(l) ? [] : prepend(first(l), take(n-1, tail(l)))

drop(n::Integer, l::List) =
  @lazy n <= 0 ? l : drop(n-1, tail(l))

takelast(n::Integer, l::List) =
  @lazy isempty(drop(n, l)) ? l : takelast(n, tail(l))

Base.last(l::List) = @>> l takelast(1) first

droplast(n::Integer, l::List) =
  map((x,_)->x, l, drop(n,l))

takenth(n::Integer, l::List) =
  @lazy isempty(l) ? [] : first(drop(n-1,l)):takenth(n, drop(n, l))

for f in [:take :drop :takelast :droplast :takenth]
    # This avoid the ambiguity with the base versions
    @eval $f(l::List, n::Int) = $f(n, l)
    @eval $f(l::List, n::Integer) = $f(n, l)
end

takewhile(pred::Function, l::List) =
  @lazy isempty(l) || !pred(first(l)) ? [] : first(l):takewhile(pred, tail(l))

dropwhile(pred::Function, l::List) =
  @lazy isempty(l) || !pred(first(l)) ? l : dropwhile(pred, tail(l))

mapply(f::@compat(Union{Function, DataType}), ls...) =
  @lazy any(isempty, ls) ? [] : prepend(f(map(first, ls)...), mapply(f, map(tail, ls)...))

# Resolves amibguity error
map(f::Function, ls::List...) = mapply(f, ls...)
map(f::DataType, ls::List...) = mapply(f, ls...)

lazymap(f::@compat(Union{Function, DataType}), ls...) = map(f, map(seq, ls)...)

@rec reduce(f::Function, v, xs::List) =
  isempty(xs) ? v : reduce(f, f(v, first(xs)), tail(xs))

reduce(f::Function, xs::List) =
  isempty(xs) ? f() : reduce(f, first(xs), tail(xs))

reductions(f::Function, v, xs::List) =
  @lazy isempty(xs) ? [] : v:reductions(f, f(v, first(xs)), tail(xs))

reductions(f::Function, xs::List) =
  @lazy isempty(xs) ? [] : reductions(f, first(xs), tail(xs))

filter(f::Function, xs::List) =
  @lazy isempty(xs) ? [] :
        f(first(xs)) ? (first(xs):filter(f, tail(xs))) :
        filter(f, tail(xs))

remove(f::Function, xs::List) = filter(x->!f(x), xs)

reverse(xs::List) = reduce((xs, x)->x:xs, list(), xs)

distinct(xs::List) = distinct(xs, Set())

distinct(xs::List, seen::Set) =
  @lazy isempty(xs) ? [] :
    first(xs) in seen ?
      distinct(tail(xs), seen) :
      first(xs):distinct(tail(xs), push!(seen, first(xs)))

function groupby(f, xs::List)
  groups = Dict()
  for x in xs
    k = f(x)
    groups[k] = x:get(groups, k, list())
  end
  return groups
end

partition(n, xs::List; step = n, pad = nothing) =
  @lazy isempty(xs) ? [] :
    @with (l = take(n, xs), len = length(l)),
      len < n ?
        (pad == nothing ? [] : list(l * take(n-len, pad))) :
        (l:partition(n, drop(step, xs); step = step, pad = pad))

partitionby(f, xs::List) =
  @lazy isempty(xs) ? [] :
    @with (x = first(xs), v = f(x), run = takewhile(x->f(x)==v, tail(xs))),
      prepend(x,run):partitionby(f, @lazy drop(length(run)+1, xs))

splitat(n, xs::List) = (take(n, xs), drop(n, xs))

splitby(p::Function, xs::List) = takewhile(p, xs), dropwhile(p, xs)

walk(inner, outer, xs::List) = @>> xs map(inner) outer
walk(inner, outer, x) = outer(x)

prewalk(f, xs)  = walk(x -> prewalk(f, x), identity, f(xs))
postwalk(f, xs) = walk(x -> postwalk(f, x), f, xs)

flatten(x) = list(x)
flatten(xs::List) = reduce((xs, x) -> xs*flatten(x), list(), xs)

# ----------
# Predicates
# ----------

import Base: any, all

==(xs::List, ys::List) =
  isempty(xs) == isempty(ys) &&
    (isempty(xs) || first(xs) == first(ys) && tail(xs) == tail(ys))

any(f::Predicate, xs::List) = @>> xs map(f) any
@rec any(xs::List) = isempty(xs) ? false : first(xs) || any(tail(xs))

all(f::Predicate, xs::List) = @>> xs map(f) all
@rec all(xs::List) = isempty(xs) ? true : first(xs) && all(tail(xs))
