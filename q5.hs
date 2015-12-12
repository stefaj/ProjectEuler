--A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

--Find the largest palindrome made from the product of two 3-digit numbers.
import Data.Map (fromListWith, toList)

is_div x y = x `mod` y == 0

evenly_divisible_by n m = and [is_div n i | i <- [1..m]]

ed n = evenly_divisible_by n 20

get_a m = filter ((flip evenly_divisible_by) m) [1..]

ans = head $ get_a 20


get_factors n 1 = get_factors n 2
get_factors 0 k = []
get_factors 1 k = [1]
get_factors n k 
		| k >= n = [k]
		| otherwise =	if is_div n k == True
						then [k] ++ get_factors (n `div` k) (k)
						else get_factors n (k+1)

is_prime n = get_factors n 1 == [n]

primes n = filter is_prime [1..n]

is_chosen k (i,j) = k == i 

chosen_freq [] p = 0
chosen_freq xs p = 	let filtered = filter (is_chosen p) xs 
					in 	if filtered == [] then 0
						else snd $ head $ filtered


frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

sum_primes :: [Integer] -> Integer -> Integer
sum_primes [] s = s
sum_primes (x:xs) s = let max_pow =  maximum [chosen_freq (frequency (get_factors i 1)) x | i <- [1..20]]
					in sum_primes xs (s * (x^max_pow))