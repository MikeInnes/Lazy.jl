# @assert choose_with_bindings(:(x = 1)) == Any[:(x = 1)]
# @assert choose_with_bindings(:(x = 1, y = 2)) == Any[:(x = 1), :(y = 2)]
# @assert choose_with_bindings(:(x, y = 1, 2)) == Any[:(y = (1, 2)), :(x = y[1]), :(y = y[2])]
# @assert choose_with_bindings(:(x, y = (1, 2))) == Any[:(y = (1, 2)), :(x = y[1]), :(y = y[2])]
# @assert choose_with_bindings(:((x, y) = 1, 2)) == Any[:(y = (1, 2)), :(x = y[1]), :(y = y[2])]
# @assert choose_with_bindings(:((x, y) = (1, 2))) == Any[:(y = (1, 2)), :(x = y[1]), :(y = y[2])]
# @assert choose_with_bindings(:(x, y = divrem(10, 3))) == Any[:(y = divrem(10, 3)), :(x = y[1]), :(y = y[2])]
# @assert choose_with_bindings(:((x, y) = divrem(10, 3))) == Any[:(y = divrem(10, 3)), :(x = y[1]), :(y = y[2])]
# @assert choose_with_bindings(:(x = y = 2)) == Any[:(x = y = 2)]

@assert (@with (x = 1), x) == 1
@assert (@with (x = 1, y = 2), (x, y)) == (1, 2)
@assert (@with (x = 1, y = 2), x + y) == 3
@assert (@with (x, y = divrem(10, 3)), (x, y)) == (3, 1)
@assert (@with ((x, y) = divrem(10, 3)), (x, y)) == (3, 1)
@assert (@with (x = y = 2), (x, y)) == (2, 2)

range_seq = seq(1:3)
@assert isa(range_seq, Lazy.LazyList)