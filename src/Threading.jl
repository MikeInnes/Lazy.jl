# Threading macros

isexpr{T}(x::T, ts...) = T in ts
isexpr(x::Expr, ts...) = x.head in ts

subexprs(ex) = filter(x -> !isexpr(x, :line), ex.args)

macro >(exs...)
  thread(x) = isexpr(x, :block) ? thread(subexprs(x)...) : x

  thread(x, ex) =
    if isexpr(ex, Symbol, :->)
      Expr(:call, ex, x)
    elseif isexpr(ex, :call, :macrocall)
      Expr(ex.head, ex.args[1], x, ex.args[2:end]...)
    elseif isexpr(ex, :block)
      thread(x, subexprs(ex)...)
    else
      error("Unsupported expression $ex in @>")
    end

  thread(x, exs...) = reduce(thread, x, exs)

  esc(thread(exs...))
end

macro >>(exs...)
  thread(x) = isexpr(x, :block) ? thread(subexprs(x)...) : x

  thread(x, ex) =
    if isexpr(ex, Symbol, :->)
      Expr(:call, ex, x)
    elseif isexpr(ex, :call, :macrocall)
      Expr(ex.head, ex.args..., x)
    elseif isexpr(ex, :block)
      thread(x, subexprs(ex)...)
    else
      error("Unsupported expression $ex in @>>")
    end

  thread(x, exs...) = reduce(thread, x, exs)

  esc(thread(exs...))
end
