is_triple (a,b,c)
		| not ((a < b) && (b < c)) = False
		| a^2 + b^2 /= c^2 = False
		| a + b + c /= 1000 = False
		| otherwise = True



perms = [(a,b,c) | a <- [200..500], b <- [200..500], c <- [280..500]]

ans = filter is_triple perms