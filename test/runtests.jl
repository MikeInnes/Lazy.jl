using Lazy
import Lazy: rest, cycle, range, drop, take
using FactCheck

facts("Lists") do
    @fact list(1, 2, 3)[2] => 2
    @fact prepend(1, list(2,3,4)) => 1:list(2, 3, 4)
    @fact seq([1, 2, 3]) => list(1, 2, 3)
    @pending seq(1:3) => list(1, 2, 3)
    @fact constantly(1)[50] => 1
    testfn() = 1
    @fact repeatedly(testfn)[50] => 1
    @fact cycle([1, 2, 3])[50] => 2
    @fact iterate(x->x^2, 2)[3] => 16
    @fact range(1, 5)[3] => 3
    @fact range(1, 5)[10] => nothing
    @fact range(1, 5)[-1] => 1
    @fact list(1, 2, 3) * list(4, 5, 6) => list(1, 2, 3, 4, 5, 6)
    @fact first(list(1, 2, 3)) => 1
    @fact rest(list(1, 2, 3)) => list(2, 3)
    @fact flatten(list(1,2,list(3,4))) => list(1, 2, 3, 4)
    @fact list(1,2,list(3,4))[3] => list(3, 4)
end

facts("Fibs") do
    fibs = @lazy 0:1:(fibs + drop(1, fibs));
    @fact fibs[20] => 4181
    @fact take(5, fibs) => list(0, 1, 1, 2, 3)
end

facts("Primes") do
    isprime(n) =
        @>> primes begin
            take_while(x -> x<=sqrt(n))
            map(x -> n % x == 0)
            any; !
        end
    primes = filter(isprime, range(2));
end

facts("Even squares") do
    esquares = @>> range() map(x->x^2) filter(iseven);
    @fact take(5, esquares) => list(4, 16, 36, 64, 100)
end

facts("Threading macros") do
  @fact ( @> 1 { +(2 + _) } ) => 3
  @fact ( @> 1 +(2, _) ) => 3
  @fact ( @> 1 +(_ + 1) ) => 3
  @fact ( @>> 1 -(0) ) => -1
end

FactCheck.exitstatus()
