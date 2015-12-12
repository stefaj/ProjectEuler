import Data.Numbers
import Data.Numbers.Primes
import Data.List
import qualified Data.Set as S

toIntegerList :: Integer -> [Integer]
toIntegerList n = map toInt (show n)
	where toInt c = read (c:"") :: Integer

digitalSum n = foldr1 (+) (toIntegerList n)


quicksort [] = []
quicksort (x:xs) = quicksort [z | z <- xs, z < x] ++ [x] ++ quicksort [z | z <- xs, z > x]

ans = [digitalSum (a^b) | a <- [1..100], b <- [1..100]]