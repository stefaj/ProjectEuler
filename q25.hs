
import Data.List


to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 

to_tens' n k = let tens = to_tens n in
			if length tens < k then [0] ++ tens else tens

digits = [0,1,2,3,4,5,6,7,8,9]

is_valid number perma = (intersect perma $ to_tens' number $ length perma) == perma



lexo_order_test = filter (flip is_valid [0,1,2]) [012..210]


lexo_order = filter (flip is_valid [0,1,2,3,4,5,6,7,8,9]) [0123456789..9876543210]
-- 123456678







quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]


ans = quicksort $ permutations "0123456789"