# Tail call operations

lastcalls(ex, f) =
  @switch isexpr(ex, _),
    :call  -> f(ex),
    :block -> Expr(:block, lastcalls(ex.args, f)...),
    :let   -> Expr(:let, lastcalls(ex.args[1], f), ex.args[2:end]...),
    :if    -> Expr(:if, ex.args[1], lastcalls(ex.args[2], f), lastcalls(ex.args[3], f)),
    :&&    -> Expr(:&&, ex.args[1], lastcalls(ex.args[2], f)),
    :||    -> Expr(:||, ex.args[1], lastcalls(ex.args[2], f)),
    ex

lastcalls(ex::Array, f) =
  isempty(ex) ? ex :
    [ex[1:end-1]..., lastcalls(ex[end], f)]

retcalls(ex, f) =
  isexpr(ex, :return) ? Expr(:return, lastcalls(ex.args[1], f)) :
  isexpr(ex) ? Expr(ex.head, map(ex->retcalls(ex, f), ex.args)...) :
  ex

tailcalls(ex, f) = @> ex lastcalls(f) retcalls(f)

# Tail recursion

export @rec, @bounce

"Generate an expression like `(a, b) = (c, d)`."
tupleassign(xs, ys) = Expr(:(=), Expr(:tuple, xs...), Expr(:tuple, ys...))

tco(ex, f, dummy, start) =
  ex.args[1] ≠ f ? ex :
    :($(tupleassign(dummy, ex.args[2:end])); @goto $start)

"""
Enables efficient recursive functions, e.g.

    @rec reduce(f::Function, v, xs::List) =
      isempty(xs) ? v : reduce(f, f(v, first(xs)), rest(xs))

Without `@rec` this function would overflow the stack for
lists of 80,000 or more elements.

Caveats:

  • No support for trampolining, i.e. only calls to the
    given function are optimised away.
  • Ignores multiple dispatch – it is assumed that the function's
    name always refers to the given definition.
  • Don't rebind the function's name in a let (see above).
  • Don't use this with varargs functions.
"""
macro rec (def)
  def = macroexpand(def)
  @assert isdef(def)
  f = namify(def)
  args = @>> def.args[1].args[2:end]
  dummy = @>> args map(namify) map(string) map(gensym)
  body = def.args[2]

  # Set up of variables
  @gensym start
  unshift!(body.args, quote
             $(tupleassign(dummy, args))
             @label $start
             $(tupleassign(args, dummy))
           end)

  def.args[2] = tailcalls(body, ex -> tco(ex, f, dummy, start))
  return esc(def)
end

# Trampolining

trampname(f) = symbol(string("#__", f, "_tramp__"))

type Bounce{T}
  f::T
end

function trampoline(f, args...)
  val = f(args...)
  while isa(val, Bounce)
    val = val.f()
  end
  return val
end

function bounce (ex)
  isexpr(ex, :call) || return ex
  f, args = ex.args[1], ex.args[2:end]
  f_tramp = trampname(f)
  :(isdefined($(Expr(:quote, f_tramp))) ?
      Lazy.Bounce(() -> $(Expr(:call, f_tramp, args...))) :
      $ex)
end

macro bounce (def)
  def = macroexpand(def)
  @assert isdef(def)
  @assert isexpr(def.args[1].args[1], Symbol) # TODO: handle f{T}() = ...
  f = namify(def)
  f_tramp = trampname(f)
  args = def.args[1].args[2:end]
  def.args[1].args[1] = f_tramp
  def.args[2] = tailcalls(def.args[2], bounce)
  quote
    $def
    $f($(args...)) = Lazy.trampoline($f_tramp, $(args...))
  end |> esc
end
