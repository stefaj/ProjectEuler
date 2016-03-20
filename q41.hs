import Data.List
import Data.Ord
import Control.Parallel.Strategies

to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 

from_tens xs = sum $ map (\(a,b) -> (10^a) * b) $ zip [0..] $ reverse xs


isPrime :: Integer -> Bool
isPrime 1 = False
isPrime k
		| k <= 0 = False 
		|otherwise = null [ x | x <- [2..(truncate $ sqrt $ fromIntegral k)], k `mod` x  == 0]


pan_primes2 = map (filter (isPrime . from_tens)) $ map permutations $ reverse [[1..n] | n <- [1..9]]

ans = maximum $ map from_tens $ concat pan_primes2




--ans = filter isPrime pans