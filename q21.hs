
-- AKS  28
-- Stefan 2


--is_ammicable n = let divsum = sum 


divsum' 1 k = 0
divsum' n 1 = 1
divsum' n k = if n `mod` k == 0 then (k + (n `div` k)) + divsum' n (k-1) else divsum' n (k-1)



divsum n = divsum' n $ truncate $ sqrt $ fromIntegral n


is_am2 1 = False
is_am2 a = let b = divsum a in
			if a /= b && divsum b == a then True else False
