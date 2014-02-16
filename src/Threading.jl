# Threading macros

thread_left(x) = thread_left(filter(x -> !isa(x,Expr) || x.head != :line, x.args)...)

function thread_left(x, expr, exprs...)
  if typeof(expr) == Symbol
    call = Expr(:call, expr, x)

  elseif typeof(expr) == Expr && expr.head in [:call, :macrocall]
    call = Expr(expr.head, expr.args[1], x, expr.args[2:]...)

  elseif typeof(expr) == Expr && expr.head == :->
    call = Expr(:call, expr, x)

  else
    error("Unsupported expression $expr in @>")
  end
  isempty(exprs) ? call : :(@> $call $(exprs...))
end

macro >(exprs...)
  esc(thread_left(exprs...))
end

macro >>(x, expr, exprs...)
  if typeof(expr) == Expr && expr.head == :tuple
    return Expr(:macrocall, symbol("@>>"), esc(x), map(esc,expr.args)..., map(esc,exprs)...)

  elseif typeof(expr) == Symbol
    call = esc(Expr(:call, expr, x))

  elseif typeof(expr) == Expr && expr.head == :call
    call = esc(Expr(:call, expr.args..., x))

  elseif typeof(expr) == Expr && expr.head == :->
      call = esc(Expr(:call, expr, x))
  else
    error("Unsupported expression $expr in @>>")
  end
  isempty(exprs) ? call : :(@>> $call $(map(esc,exprs)...))
end