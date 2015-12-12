import Data.List
import Data.Ord

itiv :: Integer -> [Integer]
itiv 1 = [1]
itiv n
	| n `mod` 2 == 0 = n : (itiv (n `div` 2))
	| otherwise = n : (itiv (3*n + 1))

chains = [length $ itiv i | i <- [1..1000000]]

maxi xs = maximumBy (comparing fst) (zip xs [0..])

ans = maxi chains