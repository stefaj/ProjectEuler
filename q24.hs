
import Data.List


to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

ans = head $ filter (\(i,x) -> (length $ show x) >= 1000) (zip [0..] fibs)