# Threading macros

export @>, @>>, @as, @_, @switch, @or, @dotimes, @oncethen, @defonce, @expand, @cond, @with, @errs,
  isexpr, namify, unblock, isdef

isexpr(x::Expr) = true
isexpr(x) = false
isexpr(x::Expr, ts...) = x.head in ts
isexpr(x, ts...) = any(T->isa(T, Type) && isa(x, T), ts)

namify(s::Symbol) = s
namify(ex::Expr) = namify(ex.args[1])

subexprs(ex) = filter(x -> !isexpr(x, :line), ex.args)

isdef(ex) = isexpr(ex, :function) || (isexpr(ex, :(=)) && isexpr(ex.args[1], :call))

function unblock(ex)
  isexpr(ex, :block) || return ex
  exs = filter(ex->!isexpr(ex, :line), ex.args)
  length(exs) == 1 || return ex
  return exs[1]
end

Base.macroexpand(m::Module, ex) =
  eval(m, :(macroexpand($(Expr(:quote, ex)))))

"""
More convenient macro expansion, e.g.

    @expand @time foo()
"""
macro expand (ex)
  :(macroexpand($(Expr(:quote, ex))))
end

"""
A switch statement of sorts:

    @switch x begin
      1; "x equals one!"
      2; "x equals two!"
      "x equals something else!"
    end

However, it's a bit more general than a regular switch in that
you can test more than just equality:

    @switch isa(x, _) begin
      Integer; "x is an integer!"
      FloatingPoint; "x is a float!"
      "x is something else!"
    end

    @switch _ begin
      a > b;  "more than!"
      a < b;  "less than!"
      a == b; "equal!"       # Note that this level of enthusiasm is not mandatory.
    end

Where `_` is replaced by the value for testing in each case. The final
expression, if there is one, is used as the default value; if there is
no default and nothing matches an error will be thrown.
"""
macro switch (args...)
  test, exprs = splitswitch(args...)
  length(exprs) == 0 && return nothing
  length(exprs) == 1 && return esc(exprs[1])

  test_expr(test, val) =
    test == :_      ? val :
    isa(test, Expr) ? :(let _ = $val; $test; end) :
                      :($test==$val)

  thread(val, yes, no) = :($(test_expr(test, val)) ? $yes : $no)
  thread(val, yes) = thread(val, yes, :(error($"No match for $test in @switch")))
  thread(val, yes, rest...) = thread(val, yes, thread(rest...))

  esc(thread(exprs...))
end

function splitswitch(test, exprs)
  @assert isexpr(exprs, :block) "@switch requires a begin block"
  test, subexprs(exprs)
end

function splitswitch(ex)
  test = ex.args[1]
  exprs = c()
  for ex in ex.args[2:end]
    isexpr(ex, :->) ?
      push!(exprs, map(unblock, ex.args)...) :
      push!(exprs, ex)
  end
  test, exprs
end

"""
The threading macro is like a more flexible version of the `|>` operator.

    @> x f = f(x)
    @> x g f == f(g(x))
    @> x a b c d e == e(d(c(b(a(x)))))

Unlike |>, functions can have arguments - the value
preceding a function will be treated as its first argument

    @> x g(y, z) f == f(g(x, y, z))

    @> x g f(y, z) == f(g(x), y, z)

See also `@>>`, `@as`.
"""
macro > (exs...)
  thread(x) = isexpr(x, :block) ? thread(subexprs(x)...) : x

  thread(x, ex) =
    isexpr(ex, Symbol, :->)       ? Expr(:call, ex, x) :
    isexpr(ex, :call, :macrocall) ? Expr(ex.head, ex.args[1], x, ex.args[2:end]...) :
    isexpr(ex, :block)            ? thread(x, subexprs(ex)...) :
                                    error("Unsupported expression $ex in @>")

  thread(x, exs...) = reduce(thread, x, exs)

  esc(thread(exs...))
end

"""
Same as `@>`, but threads the last argument.

  @>> x g(y, z) f == f(g(y, z, x))

  @>> x g f(y, z) == f(y, z, g(x))
"""
macro >> (exs...)
  thread(x) = isexpr(x, :block) ? thread(subexprs(x)...) : x

  thread(x, ex) =
    isexpr(ex, Symbol, :->)       ? Expr(:call, ex, x) :
    isexpr(ex, :call, :macrocall) ? Expr(ex.head, ex.args..., x) :
    isexpr(ex, :block)            ? thread(x, subexprs(ex)...) :
                                    error("Unsupported expression $ex in @>>")

  thread(x, exs...) = reduce(thread, x, exs)

  esc(thread(exs...))
end

"""
# @as lets you name the threaded argmument
@as _ x f(_, y) g(z, _) == g(z, f(x, y))

# All threading macros work over begin blocks

@as x 2 begin
 x^2
 x+2
end == 6

`@_` is a version of `@as` which defaults to `_` as the argument name.
"""
macro as (as, exs...)
  thread(x) = isexpr(x, :block) ? thread(subexprs(x)...) : x

  thread(x, ex) =
    isexpr(ex, Symbol, :->) ? Expr(:call, ex, x) :
    isexpr(ex, :block)      ? thread(x, subexprs(ex)...) :
    :(let $as = $x
        $ex
      end)

  thread(x, exs...) = reduce((x, ex) -> thread(x, ex), x, exs)

  esc(thread(exs...))
end

"""
Same as `@as` but uses `_` as the argmument name.
"""
macro _ (args...)
  :(@as $(esc(:_)) $(map(esc, args)...))
end

macro or (exs...)
  thread(x) = isexpr(x, :block) ? thread(subexprs(x)...) : esc(x)

  thread(x, xs...) =
    :(let x = $(esc(x))
        !(x == nothing || x == false) ? x : $(thread(xs...))
      end)

  thread(exs...)
end

"Repeat `body` `n` times."
macro dotimes (n, body)
  quote
    for i = 1:$(esc(n))
      $(esc(body))
    end
  end
end

"""
A do-while loop – executes the while loop once regardless of the
condition, then tests the condition before subsequen iterations.
"""
macro oncethen (expr::Expr)
  @assert expr.head == :while
  esc(quote
    $(expr.args[2]) # body of loop
    $expr # loop
  end)
end

"""
Stop Julia from complaining about redifined consts/types –

    @defonce type MyType
      ...
    end
    or
    @defonce const pi = 3.14
"""
macro defonce (typedef::Expr)
  name = namify(typedef.head == :type ? typedef.args[2] : typedef)

  :(if !isdefined($(Expr(:quote, name)))
      $(esc(typedef))
    end)
end

"""
End-less let block, e.g.

    @with (x = 1, y = 2),
      x+y # => 3

Also allowed multi-assignments, e.g.

    @with ((x, y) = 1, 2),
      x+y # => 3
    @with (x, y = divrem(10, 3)),
      (x, y)  # => (3, 1)
"""
macro with (ex)
  bindings, body = compose_bindings(ex.args[1]), ex.args[2]
  ex = :(let
           $body
         end)
  push!(ex.args, bindings...)
  return esc(ex)
end

function decompose_assignment{T<:Union(Symbol, Expr)}(vars::Vector, t::T)
  result = Any[]
  for (idx, lh) = enumerate(vars)
    rh = Expr(:ref, t, idx)
    if isa(lh, Symbol)
      push!(result, Expr(:(=), lh, rh))
    else
      append!(result, decompose_assignment(lh.args, rh))
    end
  end
  result
end

function compose_bindings(ex)
  src = ex.head == :tuple ? ex.args : Any[ex]
  if all(ex->(isa(ex, Expr) && ex.head == :(=) && isa(ex.args[1], Symbol)), src)
    # Simple assignments (e.g. "x = 1, y = 2")
    return src
  end
  # Decompose Complex assinment to Simples
  bindings = Any[]
  lhs = Any[]
  rhs = Any[]
  islh = true
  for ex0 = src
    if islh
      if isa(ex0, Symbol) || ex0.head != :(=)
        push!(lhs, ex0)
      else
        lh = ex0.args[1]
        if isa(lh, Symbol)
          push!(lhs, lh)
        else
          append!(lhs, lh.args)
        end
        islh = false
        push!(rhs, ex0.args[2])
      end
    else  # if isrh
      push!(rhs, ex0)
    end
  end
  rh = length(rhs) == 1 ? rhs[1] : Expr(:tuple, rhs...)
  push!(bindings, Expr(:(=), lhs[end], rh))
  append!(bindings, decompose_assignment(lhs, lhs[end]))
  return bindings
end

"""
Compile-time conditional, e.g.

    @cond VERSION > v"0.4-" ? Dict(1=>2) : [1=>2]

Also supports if-else chains via ternary or block syntax.
"""
macro cond (ex)
  ex = unblock(ex)
  isexpr(ex, :if) || return ex

  eval(current_module(), ex.args[1]) ?
    esc(ex.args[2]) :
    :(@cond $(esc(ex.args[3])))
end

# Other syntax

export c, s, @d
c(xs...) = Any[xs...]
s(xs...) = Set{Any}(xs)

macro d(xs...)
  if VERSION < v"0.4-"
    Expr(:typed_dict, :(Any=>Any), map(esc, xs)...)
  else
    :(Dict{Any, Any}($(map(esc, xs)...)))
  end
end

macro errs (ex)
  :(try $(esc(ex))
    catch e
      showerror(STDERR, e, catch_backtrace())
    end)
end
