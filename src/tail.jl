using MacroTools

# Tail call operations

function lastcalls(f, ex)
  isexpr(ex, :block) && return :(begin $(lastcalls(f, ex.args)...) end)
  @match ex begin
    let __ end     => :(let $(lastcalls(f, ex.args)...) end)
    (c_ ? y_ : n_) => :($c ? $(lastcalls(f, y)) : $(lastcalls(f, n)))
    (a_ && b_)     => :($a && $(lastcalls(f, b)))
    (a_ || b_)     => :($a || $(lastcalls(f, b)))
    _              => f(ex)
  end
end

lastcalls(f, ex::Array) =
  isempty(ex) ? ex :
    [ex[1:end-1]..., lastcalls(f, ex[end])]

function retcalls(f, ex)
  MacroTools.postwalk(ex) do ex
    @capture(ex, return x_) ? :(return $(lastcalls(f, x))) : ex
  end
end

tailcalls(f, ex) = @>> ex lastcalls(f) retcalls(f)

# Tail recursion

export @rec, @bounce

"Generate an expression like `(a, b) = (c, d)`."
tupleassign(xs, ys) = Expr(:(=), Expr(:tuple, xs...), Expr(:tuple, ys...))

tco(ex, f, dummy, start) =
  @capture(ex, $f(args__)) ?
    :($(tupleassign(dummy, args)); @goto $start) :
    ex


"""
Enables efficient recursive functions, e.g.

    @rec reduce(f::Function, v, xs::List) =
      isempty(xs) ? v : reduce(f, f(v, first(xs)), tail(xs))

Without `@rec` this function would overflow the stack for
lists of 80,000 or more elements.

Caveats:

  • No support for trampolining, i.e. only calls to the
    given function are optimised away.
  • Ignores multiple dispatch – it is assumed that the function's
    name always refers to the given definition.
  • Don't rebind the function's name in a let (see above).
  • Don't use this with varargs functions.

Use the more flexible, but slower, `@bounce` to avoid these issues.
"""
macro rec(def)
  def = shortdef(macroexpand(@__MODULE__, def))
  @capture(def, f_(args__) = body_) || error("@rec: $def is not a function definition.")
  f = namify(f)
  dummy = @>> args map(namify) map(string) map(gensym)

  # Set up of variables
  @gensym start
  body = quote
           $(tupleassign(dummy, args))
           @label $start
           $(tupleassign(args, dummy))
           $body
         end

  def.args[2] = tailcalls(ex -> tco(ex, f, dummy, start), body)
  return esc(def)
end

# Trampolining

trampname(f) = Symbol(string("#__", f, "_tramp__"))

mutable struct Bounce
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
  @isdefined(f_tramp) && return
  :($f_tramp(args...) = $f(args...))
end

"""
Tail recursion that doesn't blow the stack.

    @bounce even(n) = n == 0 ? true : odd(n-1)
    @bounce odd(n) = n == 0 ? false : even(n-1)

    even(1_000_000) # Blows up without `@bounce`.
    #> true

For simple cases you probably want the much faster `@rec`.
"""
macro bounce(def)
  def = macroexpand(def)
  @assert isdef(def)
  @assert isexpr(def.args[1].args[1], Symbol) # TODO: handle f{T}() = ...
  f = namify(def)
  f_tramp = trampname(f)
  args = def.args[1].args[2:end]
  def.args[1].args[1] = f_tramp

  calls = Symbol[]
  def.args[2] = tailcalls(ex -> (isexpr(ex, :call) && push!(calls, ex.args[1]);
                                 bounce(ex)),
                          def.args[2])
  quote
    $([trampdef(call) for call in calls]...)
    $def
    $f($(args...)) = Lazy.trampoline($f_tramp, $(args...))
  end |> esc
end
