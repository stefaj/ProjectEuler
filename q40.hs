import Data.List
import Data.Ord

to_tens :: Int -> [Int]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 


irrational = concat $ map to_tens [1..]


ans = product $ map (\x -> irrational !! (x-1)) [10^d | d <- [0..6]]