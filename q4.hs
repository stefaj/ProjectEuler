--A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

--Find the largest palindrome made from the product of two 3-digit numbers.

to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 


is_palindrome n = to_tens n == reverse (to_tens n)

digits = [i*j | i <- [100..999], j <- [100..999]]


is_prod_palin (i,j) = is_palindrome (i * j)

ans = maximum $ filter is_palindrome digits
