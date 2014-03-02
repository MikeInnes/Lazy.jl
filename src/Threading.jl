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

macro >>(x, expr, exprs...)
  if isa(expr, Expr) && expr.head == :tuple
    return Expr(:macrocall, symbol("@>>"), esc(x), map(esc,expr.args)..., map(esc,exprs)...)

  elseif typeof(expr) == Symbol
    call = esc(Expr(:call, expr, x))

  elseif isa(expr, Expr) && expr.head == :call
    call = esc(Expr(:call, expr.args..., x))

  elseif isa(expr, Expr) && expr.head == :->
      call = esc(Expr(:call, expr, x))
  else
    error("Unsupported expression $expr in @>>")
  end
  isempty(exprs) ? call : :(@>> $call $(map(esc,exprs)...))
end
