to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 


from_tens xs = sum $ map (\(a,b) -> (10^a) * b) $ zip [0..] $ reverse xs


shift xs 0 = xs
shift (x:xs) 1 = xs ++ [x]
shift (x:xs) n = (shift (xs ++ [x]) (n-1))

shift_number numb n = from_tens $ shift (to_tens numb) n


isPrime :: Integer -> Bool
isPrime 1 = False
isPrime k
		| k <= 0 = False 
		|otherwise = null [ x | x <- [2..(truncate $ sqrt $ fromIntegral k)], k `mod` x  == 0]


is_circular_prime n = 	let l = length $ to_tens n
						in
						and $ map isPrime $ map (shift_number n) [1..l]


circ_primes = filter is_circular_prime [1..1000000]