--If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

--Find the sum of all the multiples of 3 or 5 below 1000.



is_divisible x y = x `mod` y == 0

get_factors :: Integer -> Integer -> [Integer]

get_factors n 1 = get_factors n 2
get_factors 0 k = []
get_factors 1 k = [1]
get_factors n k 
		| k >= n = [k]
		| otherwise =	if is_divisible n k == True
						then [k] ++ get_factors (n `div` k) (k)
						else get_factors n (k+1)


is_prime n = get_factors n 1 == [n]

ans = last $ filter (is_prime) (get_factors 600851475143 1)