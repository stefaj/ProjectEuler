import Data.List.Unique
import Control.Monad
import Data.List

to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 

divisors :: Integer -> [Integer]
divisors n = filter (\x -> mod n x == 0) [1..n]

divisor_pairs n = zip (divisors n) (reverse $ divisors n)

--is_pandigital xs = 

frequency y [] = 0
frequency y (x:xs)
				| x == y = 1 + frequency y xs
				| otherwise = frequency y xs

all_frequency xs = map (flip frequency xs) [1..9]



-- mebe, mebe not
is_pandigital xs = ((all_frequency xs) == (take 9 $ repeat 1)) && length xs == 9





is_divisor_pair_pd (a,b) = is_pandigital (to_tens a ++ to_tens b ++ to_tens (a*b))


has_number_pandigital n = or $ map is_divisor_pair_pd $ divisor_pairs n


pan_numbers = filter has_number_pandigital [1..123456789]



pan_numbers2 = map (\(a,b) -> a*b ) $ filter is_divisor_pair_pd [(a,b) | a <- [1..2000], b <- [1..2000], b>a]



unique_numbers = (unique pan_numbers2) ++ (repeated pan_numbers2)

ans = sum unique_numbers
-- faster to work from multiplicands

-- example has no zeroes so maybe have no zeroes?


--[4396,5346,5796,6952,7254,7632,7852,15628,15678,16038,17082,17820,19084,20457,20
--754,21658,24507,26910,27504,28156,28651,32890,34902,36490,36508Interrupted.

