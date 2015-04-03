# @test choose_with_bindings(:(x = 1)) == Any[:(x = 1)]
# @test choose_with_bindings(:(x = 1, y = 2)) == Any[:(x = 1), :(y = 2)]
# @test choose_with_bindings(:(x, y = 1, 2)) == Any[:(y = (1, 2)), :(x = y[1]), :(y = y[2])]
# @test choose_with_bindings(:(x, y = (1, 2))) == Any[:(y = (1, 2)), :(x = y[1]), :(y = y[2])]
# @test choose_with_bindings(:((x, y) = 1, 2)) == Any[:(y = (1, 2)), :(x = y[1]), :(y = y[2])]
# @test choose_with_bindings(:((x, y) = (1, 2))) == Any[:(y = (1, 2)), :(x = y[1]), :(y = y[2])]
# @test choose_with_bindings(:(x, y = divrem(10, 3))) == Any[:(y = divrem(10, 3)), :(x = y[1]), :(y = y[2])]
# @test choose_with_bindings(:((x, y) = divrem(10, 3))) == Any[:(y = divrem(10, 3)), :(x = y[1]), :(y = y[2])]
# @test choose_with_bindings(:(x = y = 2)) == Any[:(x = y = 2)]

@test (@with (x = 1), x) == 1
@test (@with (x = 1, y = 2), (x, y)) == (1, 2)
@test (@with (x = 1, y = 2), x + y) == 3
@test (@with (x, y = divrem(10, 3)), (x, y)) == (3, 1)
@test (@with ((x, y) = divrem(10, 3)), (x, y)) == (3, 1)
@test (@with (x = y = 2), (x, y)) == (2, 2)

seq(1:3)
