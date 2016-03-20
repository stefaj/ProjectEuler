--If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

--Find the sum of all the multiples of 3 or 5 below 1000.

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

calc_even = calc_even = sum([fibs !! x | x <- [1..32], (fibs !! x) `mod` 2 == 0 ])

