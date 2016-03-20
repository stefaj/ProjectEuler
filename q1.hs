--If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

--Find the sum of all the multiples of 3 or 5 below 1000.


sum1 c 0 s = s
sum1 c max s = 	if ((c `mod` 3 == 0) || (c `mod` 5 == 0 ))
				then sum1 (c+1) (max-1) (s+c)
				else sum1 (c+1) (max-1) (s)