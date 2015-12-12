
-- AKS  28
-- Stefan 2


--is_ammicable n = let divsum = sum 


divsum' :: Int -> Int -> Int
divsum' 1 k = 0
divsum' n 1 = 1
divsum' n k
		| (fromIntegral k == (sqrt $ fromIntegral n) && n `mod` k == 0) = k + divsum' n (k-1)
		| n `mod` k == 0 = (k + (n `div` k)) + divsum' n (k-1) 
		| otherwise = divsum' n (k-1)




is_abundant' :: Int -> Int -> Int -> Bool
is_abundant' 1 k acc = False
is_abundant' n 1 acc = acc > n
is_abundant' n k acc
		| n < 12 = False
		| acc > n = True
		| (fromIntegral k == (sqrt $ fromIntegral n) && n `mod` k == 0) = is_abundant' n (k-1) (acc + k)
		| n `mod` k == 0 = is_abundant' n (k-1) (acc + k + (n `div` k))
		| otherwise = is_abundant' n (k-1) acc
is_abundant2 n = is_abundant' n (truncate $ sqrt $ fromIntegral n) 0





divsum n = divsum' n $ truncate $ sqrt $ fromIntegral n


is_perfect n = divsum n == n
is_deficient n = divsum n < n
is_abundant n = divsum n > n


abundants = filter is_abundant [1..28123]

abundant_sums = [(x+y) | x <- abundants, y <- abundants, y >= x]
kwl_numbers = [x | x <- [1..28123], not (x `elem` abundant_sums)]


