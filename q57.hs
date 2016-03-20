import Data.Ratio


toIntegerList :: Integer -> [Integer]
toIntegerList n = map toInt (show n)
	where toInt c = read (c:"") :: Integer

hasCondition r = let
	numDigits = length $ toIntegerList $ numerator r
	denomDigits = length $ toIntegerList $ denominator r
	in numDigits > denomDigits

squareExpans :: Integer -> Ratio Integer
squareExpans n = 1 + 1/(x n)
	where
		x 1 = 2
		x n = (2 + 1/(x (n-1)))


ans' = filter hasCondition [squareExpans n | n <- [1..1000]]
ans = length ans'











































