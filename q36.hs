to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 

to_binary :: Integer -> [Integer]
to_binary 0 = []
to_binary n = to_binary (n `div` 2) ++ [n `mod` 2] 

is_palindrome xs = xs == (reverse xs)


is_palindrome_base n = is_palindrome (to_tens n) && is_palindrome (to_binary n)


ans = sum $ filter is_palindrome_base [1..1000000]