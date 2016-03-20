triangle n = (n * (n+1)) `div` 2

triangles = map triangle [1..]

divisors :: Integer -> [Integer]
divisors n = filter (\x -> mod n x == 0) [1..n]

divisors2 :: Integer -> Integer
divisors2 n = 	let aux i = if i*i < n then 2 else 1
				in sum [aux i | i <- [1..truncate (sqrt $ fromInteger n)], n `mod` i == 0]

--ans = [x | i <- [1..10], let x = triangle i, length $ divisors x > 5]
ans = head $ filter (\y -> divisors2 y > 500) triangles