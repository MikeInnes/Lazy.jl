# Threading macros

export @>, @>>, @as

isexpr(x::Expr, ts...) = x.head in ts
isexpr{T}(x::T, ts...) = T in ts

subexprs(ex) = filter(x -> !isexpr(x, :line), ex.args)

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
