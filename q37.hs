to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime k
		| k <= 0 = False 
		|otherwise = null [ x | x <- [2..(truncate $ sqrt $ fromIntegral k)], k `mod` x  == 0]


from_tens xs = sum $ map (\(a,b) -> 10^a * b) $ zip [0..] $ reverse xs


truncate_left' xs 0 = xs
truncate_left' (x:[]) _ = []
truncate_left' (x:xs) 1 = xs
truncate_left' (x:xs) n = (truncate_left' (xs) (n-1))
truncate_left num n = from_tens $ truncate_left' (to_tens num) n


truncate_right num n = 	let xs = to_tens num
						in from_tens $ reverse $ truncate_left' (reverse xs) n 



is_truncatable_prime 2 = False
is_truncatable_prime 3 = False
is_truncatable_prime 5 = False
is_truncatable_prime 7 = False
is_truncatable_prime n = 	let 
								l = length $ to_tens n
							in
							and $ map isPrime $ map (truncate_left n) [0..(l-1)] ++ map (truncate_right n) [0..(l-1)]

-- ++ ((truncate_left n) [0..(l-1)]))


ans = filter is_truncatable_prime [x| x <- [11,13..739397], isPrime x]