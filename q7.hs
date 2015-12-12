is_div x y = x `mod` y == 0


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

primes_inf = filter is_prime [1,3..]