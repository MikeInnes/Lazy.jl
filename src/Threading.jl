# Threading macros

export @>, @>>, @as, @cond

isexpr(x::Expr, ts...) = x.head in ts
isexpr{T}(x::T, ts...) = T in ts

subexprs(ex) = filter(x -> !isexpr(x, :line), ex.args)

macro cond(test, exprs)
  @assert isexpr(exprs, :block) "@cond requires a begin block"
  exprs = subexprs(exprs)
  length(exprs) == 0 && return nothing
  
  test_expr(test, val) = isexpr(test, Symbol) ? :($test($val)) : :(let _ = $val; $test; end)
  
  thread(test, val, yes, no) = :($(test_expr(test, val)) ? $yes : $no)
  thread(test, val, yes) = thread(test, val, yes, :(error("No match in @cond")))
  thread(test, val, yes, rest...) = thread(test, val, yes, thread(test, rest...))
  
  esc(thread(test, exprs...))
end

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

macro as (exs...)
  thread(as, x) = isexpr(x, :block) ? thread(as, subexprs(x)...) : x

  thread(as, x, ex) =
    isexpr(ex, Symbol, :->) ? Expr(:call, ex, x) :
    isexpr(ex, :block)      ? thread(as, x, subexprs(ex)...) :
    :(let $as = $x
        $ex
      end)

  thread(as, x, exs...) = reduce((x, ex) -> thread(as, x, ex), x, exs)

  esc(thread(exs...))
end
