to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 



fac 0 = 1
fac 1 = 1
fac n = product [1..n]


is_curious 1 = False
is_curious 2 = False
is_curious n = (sum $ map fac $ to_tens n) == n






cui = filter is_curious [1..]