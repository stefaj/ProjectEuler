import Data.List
import Data.Maybe
import Data.List.Split

pans = permutations [0,1,2,3,4,5,6,7,8,9]

from_tens xs = sum $ map (\(a,b) -> (10^a) * b) $ zip [0..] $ reverse xs

to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 



satisfies_constraints d = 
						from_tens [(d!!1), (d!!2), (d!!3)] `mod` 2 == 0 && 
						from_tens [(d!!2), (d!!3), (d!!4)] `mod` 3 == 0 && 
						from_tens [(d!!3), (d!!4), (d!!5)] `mod` 5 == 0 && 
						from_tens [(d!!4), (d!!5), (d!!6)] `mod` 7 == 0 && 
						from_tens [(d!!5), (d!!6), (d!!7)] `mod` 11 == 0 && 
						from_tens [(d!!6), (d!!7), (d!!8)] `mod` 13 == 0 && 
						from_tens [(d!!7), (d!!8), (d!!9)] `mod` 17 == 0

wierd_numbers = map from_tens $ filter satisfies_constraints pans
