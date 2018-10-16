using MacroTools

# Threading macros

import Base: replace

export @>, @>>, @as, @_, @switch, @or, @dotimes, @oncethen, @defonce, @with, @errs,
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

  @static if VERSION < v"0.7"
  
    thread(x, ex) =
    isexpr(ex, :call, :macrocall) ? Expr(ex.head, ex.args[1], x, ex.args[2:end]...) :
    @capture(ex, f_.(xs__))       ? :($f.($x, $(xs...))) :
    isexpr(ex, :block)            ? thread(x, rmlines(ex).args...) :
    Expr(:call, ex, x)

  else

    thread(x, ex) =
    isexpr(ex, :macrocall)        ? Expr(ex.head, ex.args[1], ex.args[2], x, ex.args[3:end]...) :
    isexpr(ex, :call,)            ? Expr(ex.head, ex.args[1], x, ex.args[2:end]...) :
    @capture(ex, f_.(xs__))       ? :($f.($x, $(xs...))) :
    isexpr(ex, :block)            ? thread(x, rmlines(ex).args...) :
    Expr(:call, ex, x)

  end

  thread(x, exs...) = reduce(thread, exs, init=x)
    
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
    @capture(ex, f_.(xs__))       ? :($f.($(xs...), $x)) :
    isexpr(ex, :block)            ? thread(x, rmlines(ex).args...) :
                                    error("Unsupported expression $ex in @>>")

  thread(x, exs...) = reduce(thread, exs; init=x)

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

  thread(x, exs...) = reduce((x, ex) -> thread(x, ex), exs, init=x)

  esc(thread(exs...))
end

@static if VERSION < v"0.7"
  """
  Same as `@as` but uses `_` as the argmument name.
  """
  macro _(args...)
    :(@as $(esc(:_)) $(map(esc, args)...))
  end
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
  ex = :(let $(bindings...)
           $body
         end)
  return esc(ex)
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
  :(Dict{Any, Any}($(map(esc, xs)...)))
end

macro errs(ex)
  :(try $(esc(ex))
    catch e
      showerror(stderr, e, catch_backtrace())
      println(stderr)
    end)
end

"""
    @forward T.x functions...

Define methods for `functions` on type `T`, which call the relevant function
on the field `x`.

# Example

```julia
struct Wrapper
    x
end

@forward Wrapper.x  Base.sqrt                                  # now sqrt(Wrapper(4.0)) == 2.0
@forward Wrapper.x  Base.length, Base.getindex, Base.iterate   # several forwarded functions are put in a tuple
@forward Wrapper.x (Base.length, Base.getindex, Base.iterate)  # equivalent to above
```
"""
macro forward(ex, fs)
  @capture(ex, T_.field_) || error("Syntax: @forward T.x f, g, h")
  T = esc(T)
  fs = isexpr(fs, :tuple) ? map(esc, fs.args) : [esc(fs)]
  :($([:($f(x::$T, args...) = (Base.@_inline_meta; $f(x.$field, args...)))
       for f in fs]...);
    nothing)
end

# Forwarding iteration

struct SubIter{I,S}
  iter::I
  state::S
end

# Julia#16096

macro iter(ex)
  @capture(ex, x_::T_ -> it_) || error("Use @iter x::T -> y ...")
  @capture(it, $x.f_) &&
    return :(@forward $(esc(T)).$f Base.iterate, Base.iterate)
  quote
    @inline function Base.iterate($x::$T)
      it = $it
      Lazy.SubIter(it, Base.iterate(it))
    end
    @inline function Base.iterate(::$T, sub::Lazy.SubIter)
      next, state = Base.iterate(sub.iter, sub.state)
      next == nothing && return nothing
      next, Lazy.SubIter(sub.iter, state)
    end
  end |> esc
end

# Init macro

export @init

function initm(ex)
  quote

    if !isdefined(@__MODULE__, :__inits__)
      const $(esc(:__inits__)) = Function[]
    end
    if !isdefined(@__MODULE__, :__init__)
      function $(esc(:__init__))()
        for f in $(esc(:__inits__))
          f()
        end
      end
    end

    push!($(esc(:__inits__)), () -> $(esc(ex)))
    nothing
  end
end

macro init(args...)
  initm(args...)
end
