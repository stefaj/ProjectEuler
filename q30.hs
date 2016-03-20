to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 


is_num_valid 1 = False
is_num_valid num = 		let tens = to_tens num
						in (sum $ map (\x -> x^5) tens) == num


