@assert (@with (x = 1), x) == 1
@assert (@with (x = 1, y = 2), (x, y)) == (1, 2)

range_seq = seq(1:3)
@assert isa(range_seq, Lazy.LazyList)
@assert collect(range_seq) == [1,2,3]