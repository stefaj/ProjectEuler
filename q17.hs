import Data.List

wordnames 1 = "one"
wordnames 2 = "two"
wordnames 3 = "three"
wordnames 4 = "four"
wordnames 5 = "five"
wordnames 6 = "six"
wordnames 7 = "seven"
wordnames 8 = "eight"
wordnames 9 = "nine"

wordnames 11 = "eleven"
wordnames 12 = "twelve"
wordnames 13 = "thirteen"
wordnames 14 = "fourteen"
wordnames 15 = "fifteen"
wordnames 16 = "sixteen"
wordnames 17 = "seventeen"
wordnames 18 = "eighteen"
wordnames 19 = "nineteen"

wordnames 10 = "ten"
wordnames 20 = "twenty"
wordnames 30 = "thirty"
wordnames 40 = "forty"
wordnames 50 = "fifty"
wordnames 60 = "sixty"
wordnames 70 = "seventy"
wordnames 80 = "eighty"
wordnames 90 = "ninety"

wordnames 100 = "onehundred"
wordnames 200 = "twohundred"
wordnames 300 = "threehundred"
wordnames 400 = "fourhundred"
wordnames 500 = "fivehundred"
wordnames 600 = "sixhundred"
wordnames 700 = "sevenhundred"
wordnames 800 = "eighthundred"
wordnames 900 = "ninehundred"
wordnames 1000 = "onethousand"
wordnames n = ""


to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 

decomp_word n = reverse $ zipWith (\x y -> x*10^y ) (reverse $ to_tens n) [0..]
fix_decomp [] = []
fix_decomp (x:[]) = [x]
fix_decomp (x:xs)
			| x>=10 && x<20 = [x + head xs]
			| otherwise = x : fix_decomp xs

decompose_word n = fix_decomp $ decomp_word n

custom_word_add x "" = x
custom_word_add "" y = y
custom_word_add x y = if isInfixOf "hundred" x then x ++ "and" ++ y else x ++ y

write_word n = foldr (custom_word_add) [] (map wordnames (decompose_word n))

length_of_written n = (length $ write_word n)

ans = sum [length_of_written i | i <- [1..1000]]
--threehundredandfourtytwo
--three hundred and forty-two