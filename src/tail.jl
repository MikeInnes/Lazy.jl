using MacroTools

# Tail call operations

function lastcalls(ex, f)
  @match ex begin
    g_(__)         => f(ex)
    begin __ end   => :(begin $(lastcalls(ex.args, f)...) end)
    let __ end     => :(let $(lastcalls(ex.args, f)...) end)
    (c_ ? y_ : n_) => :($c ? $(lastcalls(y, f)) : $(lastcalls(n, f)))
    a_ && b_       => :($a && $(lastcalls(b, f)))
    a_ || b_       => :($a || $(lastcalls(b, f)))
    _              => ex
  end
end

lastcalls(ex::Array, f) =
  isempty(ex) ? ex :
    [ex[1:end-1]..., lastcalls(ex[end], f)]

function retcalls(ex, f)
  @match ex begin
    (return x_) => :(return $(lastcalls(x, f)))
    _Expr       => Expr(ex.head, map(ex->retcalls(ex, f), ex.args)...)
    _           => ex
  end
end

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
macro rec(def)
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

type Bounce
  f::Function
end

function trampoline(f, args...)
  val = f(args...)
  while isa(val, Bounce)
    val = val.f()
  end
  return val
end

function bounce(ex)
  @capture(ex, f_(args__)) || return ex
  f_tramp = trampname(f)
  :(Lazy.Bounce(() -> $f_tramp($(args...))))
end

function trampdef(f)
  f_tramp = trampname(f)
  isdefined(f_tramp) && return
  :($f_tramp(args...) = $f(args...))
end

macro bounce(def)
  def = macroexpand(def)
  @assert isdef(def)
  @assert isexpr(def.args[1].args[1], Symbol) # TODO: handle f{T}() = ...
  f = namify(def)
  f_tramp = trampname(f)
  args = def.args[1].args[2:end]
  def.args[1].args[1] = f_tramp

  calls = Symbol[]
  def.args[2] = tailcalls(def.args[2],
                          ex -> (isexpr(ex, :call) && push!(calls, ex.args[1]);
                                 bounce(ex)))
  quote
    $([trampdef(call) for call in calls]...)
    $def
    $f($(args...)) = Lazy.trampoline($f_tramp, $(args...))
  end |> esc
end
