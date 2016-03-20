import Data.List

to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 


cancel_like_digits (a,b) = let 	
								top = to_tens a
								bot = to_tens b
								same = intersect top bot
								new_top = top \\ same
								new_bot = bot \\ same
							in 
								if length same > 0 && length new_top > 0 && length new_bot > 0
								then (head new_top, head new_bot)
								else (a,b)


normalize_frac (a,b) = let g = gcd a b in (a `div` g, b `div` g)

is_frac_equal (a',b') (c',d') = let
									(a,b) = normalize_frac (a',b')
									(c,d) = normalize_frac (c',d')
								in a==c && b==d 


is_cancelled_correct (a,b) = 	let (q,w) = cancel_like_digits (a,b) 
								in if (q==a && w == b) 
									then False
									else is_frac_equal (q,w) (a,b)


combs = [(a,b) | a <- [1..100], b <- [1..100], b > a]



curious_fracs = filter (\(a,b) -> (a `mod` 10) /= 0 && (b `mod` 10) /= 0 )  $ filter is_cancelled_correct combs


lowest_gryffindor_common_room = map normalize_frac curious_fracs

ans = foldr (\(a,b) (c,d) -> (a*c,b*d)) (1,1) lowest_gryffindor_common_room


-- normalize_frac $ 