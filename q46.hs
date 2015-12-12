import Data.Maybe





is_composite :: Int -> Bool
is_composite n = or $ map (\x -> n `mod` x == 0) [2..(truncate $ sqrt $ fromIntegral n)]


is_prime = not . is_composite


is_composite_odd n = odd n && is_composite n

closest_prime n = head $ filter is_prime [n-1,n-2..2]

remainder t n s
			| n + 2*s*s > t 	=	Nothing
			| n + 2*s*s == t	=	Just s
			| otherwise			= 	remainder t n (s+1) 



best_combo t 2 = Nothing
best_combo t p = 	let 
						p' = closest_prime p
						res = remainder t p 0
					in
						if res == Nothing then best_combo t p'
						else Just (t,p, fromJust res)

goldbach_triple n = best_combo n (n-1)


golds = [(x,y) | x <- [9..], let y = goldbach_triple x, is_composite_odd x]

failed = filter (\(x,y) -> y ==Nothing) golds






--ans = head $ filter (Nothing==) [y | x <- [1..], let y = map (\z -> remainder z (closest_prime z) 0) x, is_composite_odd x]