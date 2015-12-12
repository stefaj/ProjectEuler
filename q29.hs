import Data.List.Unique

combos n = [a^b | a <- [2..n], b <- [2..n]]

ans = unique $ combos 100

real_ans = length ans


quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]



get_all n = let c = combos n
			in quicksort $ (unique c) ++ (repeated c)


test1 = get_all 5 == [4,8,9,16,25,27,32,64,81,125,243,256,625,1024,3125]