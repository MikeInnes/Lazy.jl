using MacroTools

# Threading macros

import Base: replace

export @>, @>>, @as, @_, @switch, @or, @dotimes, @oncethen, @defonce, @cond, @with, @errs,
  @forward, @iter

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
macro switch(args...)
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
  test, rmlines(exprs).args
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
macro >(exs...)
  thread(x) = isexpr(x, :block) ? thread(rmlines(x).args...) : x

  thread(x, ex) =
    isexpr(ex, :call, :macrocall) ? Expr(ex.head, ex.args[1], x, ex.args[2:end]...) :
    isexpr(ex, :block)            ? thread(x, rmlines(ex).args...) :
    Expr(:call, ex, x)

  thread(x, exs...) = reduce(thread, x, exs)

  esc(thread(exs...))
end

"""
Same as `@>`, but threads the last argument.

  @>> x g(y, z) f == f(g(y, z, x))

  @>> x g f(y, z) == f(y, z, g(x))
"""
macro >>(exs...)
  thread(x) = isexpr(x, :block) ? thread(rmlines(x).args...) : x

  thread(x, ex) =
    isexpr(ex, Symbol, :->)       ? Expr(:call, ex, x) :
    isexpr(ex, :call, :macrocall) ? Expr(ex.head, ex.args..., x) :
    isexpr(ex, :block)            ? thread(x, rmlines(ex).args...) :
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
macro as(as, exs...)
  thread(x) = isexpr(x, :block) ? thread(rmlines(x).args...) : x

  thread(x, ex) =
    isexpr(ex, Symbol, :->) ? Expr(:call, ex, x) :
    isexpr(ex, :block)      ? thread(x, rmlines(ex).args...) :
    :(let $as = $x
        $ex
      end)

  thread(x, exs...) = reduce((x, ex) -> thread(x, ex), x, exs)

  esc(thread(exs...))
end

"""
Same as `@as` but uses `_` as the argmument name.
"""
macro _(args...)
  :(@as $(esc(:_)) $(map(esc, args)...))
end

macro or(exs...)
  thread(x) = isexpr(x, :block) ? thread(rmlines(x).args...) : esc(x)

  thread(x, xs...) =
    :(let x = $(esc(x))
        !(x == nothing || x == false) ? x : $(thread(xs...))
      end)

  thread(exs...)
end

"Repeat `body` `n` times."
macro dotimes(n, body)
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
macro oncethen(expr::Expr)
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
macro defonce(def)
  name = namify(isexpr(def, :type) ? def.args[2] : def)

  :(if !isdefined($(Expr(:quote, name)))
      $(esc(def))
    end)
end

"""
End-less let block, e.g.

    @with (x = 1, y = 2),
      x+y
"""
macro with(ex)
  @capture(ex, ((bindings__,), body_)) || error("Invalid expression @with $ex")
  ex = :(let
           $body
         end)
  push!(ex.args, bindings...)
  return esc(ex)
end

"""
Compile-time conditional, e.g.

    @cond VERSION > v"0.4-" ? Dict(1=>2) : [1=>2]

Also supports if-else chains via ternary or block syntax.
"""
macro cond(ex)
  @match ex begin
    (c_ ? y_ : n_) => (eval(current_module(), c) ? esc(y) : :(@cond $(esc(n))))
    _ => esc(ex)
  end
end

# Other syntax

export c, s, d, @d
c(xs...) = Any[xs...]
s(xs...) = Set{Any}(xs)
d(xs...) = Dict{Any, Any}(xs...)

"""
Creates a typed dictionary, e.g.

   julia> @d(a=>1,b=>2)
   Dict{Any,Any} with 2 entries:
     :a => 1
     :b => 2
"""
macro d(xs...)
  @cond if VERSION < v"0.4-"
    Expr(:typed_dict, :(Any=>Any), map(esc, xs)...)
  else
    :(Dict{Any, Any}($(map(esc, xs)...)))
  end
end

macro errs(ex)
  :(try $(esc(ex))
    catch e
      showerror(STDERR, e, catch_backtrace())
      println(STDERR)
    end)
end

macro forward(ex, fs)
  @capture(ex, T_.field_) || error("Syntax: @forward T.x f, g, h")
  T = esc(T)
  fs = isexpr(fs, :tuple) ? map(esc, fs.args) : [esc(fs)]
  :($([:($f(x::$T, args...) = (Base.@_inline_meta; $f(x.$field, args...)))
       for f in fs]...);
    nothing)
end

# Forwarding iteration

immutable SubIter{I,S}
  iter::I
  state::S
end

# Julia#16096

macro iter(ex)
  @capture(ex, x_::T_ -> it_) || error("Use @iter x::T -> y ...")
  @capture(it, $x.f_) &&
    return :(@forward $(esc(T)).$f Base.start, Base.next, Base.done)
  quote
    @inline function Base.start($x::$T)
      it = $it
      Lazy.SubIter(it, Base.start(it))
    end
    @inline function Base.next(::$T, sub::Lazy.SubIter)
      next, state = Base.next(sub.iter, sub.state)
      next, Lazy.SubIter(sub.iter, state)
    end
    @inline function Base.done(::$T, sub::Lazy.SubIter)
      Base.done(sub.iter, sub.state)
    end
  end |> esc
end

# Init macro

export @init

macro definit()
  quote
    if !isdefined(:__inits__)
      const $(esc(:__inits__)) = Function[]
    end
    if !isdefined(:__init__)
      $(esc(:__init__))() = @init
    end
  end
end

function initm(ex)
  quote
    @definit
    push!($(esc(:__inits__)), () -> $(esc(ex)))
    nothing
  end
end

function initm()
  :(for f in __inits__
      f()
    end) |> esc
end

macro init(args...)
  initm(args...)
end
