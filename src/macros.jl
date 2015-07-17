using MacroTools

# Threading macros

import Base: replace

export @>, @>>, @as, @_, @switch, @or, @dotimes, @oncethen, @defonce, @cond, @with, @errs

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
@as_first always conducts substitution at all locations of its first argument, the as-token (_ in these examples).

@as_first _ 1 +(1, _) will translate to +(1, 1)

Insertion is a default behavior.

@as_first _ 1 +(1) will translate to +(1, 1). 1 was piped in as the first argument

Insertion can be overriden in two ways:

1) Wrapping in curly brackets

@as_first _ 1 { +(1, 1 + _) } will translate to +(1, 1 + 1) (only substitution occured)

2) Including a bare as-token as an argument

@as_first _ 1 +(1, _) will translate to +(1, 1) (identical to the first example)
"""
macro as_first(as_token, code...)
  thread(single_expression) =
    # split up a quoted expression into its lines
    if isexpr(single_expression, :block)
      thread(rmlines(single_expression).args...)
      # or else no chaining necessary
    else
      single_expression
    end

  thread(chain, next) =

    # if the next expression is a lambda, call the function on the chain
    if  isexpr(next, Symbol, :->)
      Expr(:call, next, chain)

      # split up quote blocks
    elseif isexpr(next, :block)
      thread(chain, rmlines(next).args...)

      # if the expression is wrapped in curly brackets, use inside and substitute token
    elseif isexpr(next, :cell1d)
      next = next.args[1]
      :(
        let $as_token = $chain
          $next
        end
      )

      # if the token is an argument by itself, substitute token
    elseif as_token in next.args
       :(
        let $as_token = $chain
          $next
        end
      )

      # if the next expression is a function or a macro
    elseif isexpr(next, :call, :macrocall)
      # insert chain as first argument
      next =
        Expr(next.head,
           next.args[1],
           chain,
           next.args[2:end]...)

      # substitute token
      :(
          let $as_token = $chain
            $next
          end
        )
    else
      error("Unsupported expression $next")
    end

  # default to the first expression if there is no remaining code

  thread(only, code...) = reduce((only, code) -> thread(only, code), only, code)

  esc(thread(code...))
end

"""
Same as @as_first, except the chain is inserted as the last argument

@as_first _ 1 -(0) is -(0, 1)
"""
macro as_last(as_token, code...)
  thread(single_expression) =
    # split up a quoted expression into its lines
    if isexpr(single_expression, :block)
      thread(rmlines(single_expression).args...)
      # or else no chaining necessary
    else
      single_expression
    end

  thread(chain, next) =

    # if the next expression is a lambda, call the function on the chain
    if  isexpr(next, Symbol, :->)
      Expr(:call, next, chain)

      # split up quote blocks
    elseif isexpr(next, :block)
      thread(chain, rmlines(next).args...)

      # if the expression is wrapped in curly brackets, use inside and substitute token
    elseif isexpr(next, :cell1d)
      next = next.args[1]
      :(
        let $as_token = $chain
          $next
        end
      )

      # if the token is an argument by itself, substitute token
    elseif as_token in next.args
       :(
        let $as_token = $chain
          $next
        end
      )

      # if the next expression is a function or a macro
    elseif isexpr(next, :call, :macrocall)

      # insert chain as last argument
      next =
        Expr(next.head,
             next.args...,
             chain)

      # substitute token
      :(
          let $as_token = $chain
            $next
          end
        )
    else
      error("Unsupported expression $next")
    end

  # default to the first expression if there is no remaining code

  thread(only, code...) = reduce((only, code) -> thread(only, code), only, code)

  esc(thread(code...))
end

"""
Same as @as_first, but the as-token is _ automatically.

@> 1 +(1, _) = +(1, 1)
"""
macro >(args...)
  :(@as_first $(esc(:_)) $(map(esc, args)...))
end

"""
Same as @as_last, but the as-token is _ automatically.

@>> 1 -(0, _) = +(1, 1)
"""
macro >>(args...)
  :(@as_last $(esc(:_)) $(map(esc, args)...))
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
    (c_ ? y_ : n_) => eval(current_module(), c) ? esc(y) : :(@cond $(esc(n)))
    _ => esc(ex)
  end
end

# Other syntax

export c, s, @d
c(xs...) = Any[xs...]
s(xs...) = Set{Any}(xs)

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
    end)
end
