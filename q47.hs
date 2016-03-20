import qualified Data.Set as S
import Data.List

import qualified Data.Map as M

import Data.List
import Data.Numbers
import Data.Numbers.Primes
import qualified Data.Set as Set


get_factors :: Integer -> Integer -> [Integer]
get_factors n 1 = get_factors n 2
get_factors 0 k = []
get_factors 1 k = []
--get_factors n k 
--		| k > (truncate $ sqrt $ fromIntegral n) 	= []
--		| otherwise =	if n `mod` k == 0
--						then [k] ++ [n `div` k] ++ get_factors (n `div` k) (k)
--						else get_factors n (k+1)



get_factors n k 
		| k >= n = [k]
		| otherwise =	if n `mod` k == 0
						then [k] ++ get_factors (n `div` k) (k)
						else get_factors n (k+1)



distinct_factors n = length $ S.elems $ S.fromAscList $ get_factors n 1
--distinct_factors2 n = length $ S.elems $ S.fromAscList $ primeFactors n 1



four_consecs = [x | x <- [100000..],  (distinct_factors x == 4) && (distinct_factors (x+1) == 4)
										&& (distinct_factors (x+2) == 4) && (distinct_factors (x+3) == 4)]


ans = head four_consecs

three_consecs = [x | x <- [15..], and $ map (\z -> distinct_factors z == 3) [x+y | y <- [0..2]]]




divisors2 :: Integer -> Integer
divisors2 n = 	let aux i = if i*i < n then 2 else 1
				in sum [aux i | i <- [1..truncate (sqrt $ fromInteger n)], n `mod` i == 0]

