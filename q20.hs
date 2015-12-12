fact n = product [1..n]

to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 


ans = sum $ to_tens $ fact 100