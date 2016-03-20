import Data.Numbers
import Data.Numbers.Primes
import Data.List
import qualified Data.Set as S

fact 1 = 1
fact 0 = 1
fact n = product [1..n]


nCr n r = (fact n) `div` (fact r * fact (n-r))






allPos = [nCr n r | n <- [1..100], r <- [1..n]]
mill = filter (>1000000) allPos
ans = length mill