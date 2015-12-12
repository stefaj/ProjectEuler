import Data.Numbers
import Data.Numbers.Primes
import Data.List
import qualified Data.Set as S

from_tens xs = sum $ map (\(a,b) -> (10^a) * b) $ zip [0..] $ reverse xs

to_tens :: Integer -> [Integer]
to_tens 0 = []
to_tens n = to_tens (n `div` 10) ++ [n `mod` 10] 

--isPrime :: Integer -> Bool
--isPrime 1 = False
--isPrime k
--		| k <= 0 = False 
--		|otherwise = null [ x | x <- [2..(truncate $ sqrt $ fromIntegral k)], k `mod` x  == 0]
nprimes xs = length $ filter isPrime xs

ch='1'
numChar n= sum [1|x<-show(n),x==ch]
replace d c|c==ch=d
           |otherwise=c
nextN repl n= (+0)$read $map repl $show n  
same n= [if isPrime$nextN (replace a) n then 1 else 0|a<-['1'..'9']]
problem_51=head [n|
    n<-[100003,100005..999999],
    numChar n==3,
    (sum $same n)==8
    ]


isSame (x:xs) = and $ map (x==) xs

thecombohaha = [[a,b,c,d,e,f] | a <- [0,1], b <- [0,1], c <- [0,1], d <- [0,1], e <- [0,1], f <- [0,1], not $ isSame [a,b,c,d,e,f], let l = a+b+c+d+e+f, l > 1, l < 4]

applyMask [] [] _ = []
applyMask xs ys _
			| length xs /= (length ys) = []
applyMask (x:xs) (y:ys) c
					| y == 0 = x : applyMask xs ys x
					| y == 1 = c : applyMask xs ys x
					| otherwise = error $ (show (x:xs)) ++ " " ++ (show (y:ys)) ++ " " ++ (show c) ++ " ha"



thecombohaha2 = [[a,b,c,d,e,f] | a <- [0,1], b <- [0,1], c <- [0,1], d <- [0,1], e <- [0,1], f <- [0,1], a+b+c+d+e+f == 2]


possibilities = [x | x <- takeWhile (<9999999) primes, x > 99999]
poss3 = [[applyMask (to_tens x) y c | y <- thecombohaha2, c <- [0..9] ] |  x <- possibilities]
poss4 = map (\xs -> filter (\x -> length (show x) == 6) $ nub $ filter isPrime $ map (\ys -> from_tens ys) xs) poss3

sortL zs = sortBy (\xs ys -> (nprimes ys) `compare` (nprimes xs)) zs

mostPrimes = head $ sortL poss4
primesOf8 = filter (\xs -> nprimes xs == 8 ) poss4



--combo5 = [ [from_tens [a,b,c,d,e] | c <- [0..9], d<- [0..9], c==d ] | a <- [1..9], b <- [0..9], e <- [0..9]]
--ans5 = head $ sortL combs5



--permuteNumber n = map from_tens $ permutations $ to_tens n

--combo6 = [ [ from_tens [a,b,c,d,e,f] | c <- [0..9], d<- [0..9], c==d ] | a <- [1..9], b <- [0..9], e <- [0..9], f <- [0..9]]





-- from_tens $ funT combs2 = [[a,b] | a <- [0,1], b <- [0,1], a /= b]
--combs3 = [[a,b,c] | a <- [0,1], b <- [0,1], c <- [0,1], not $ isSame [a,b,c]]
--combs5 = [[a,b,c,d,e] | a <- [0,1], b <- [0,1], c <- [0,1], d <- [0,1], e <- [0,1],  not $ isSame [a,b,c,d,e]]

--testTup = [1,1,1,0,1,0]

--funT [] _ _ = []
--funT (x:xs) a b = if x == 1 then a : funT xs a b else b : funT xs a b

--b = [ [  from_tens $ funT xs a b | a <- [0..9] ] |  xs <- combs2, b <- [1..9] ]
--c = [ [  from_tens $ funT xs a b | a <- [0..9] ] |  xs <- combs3, b <- [1..9] ]
--d = [ [  from_tens $ funT xs a b | a <- [0..9] ] |  xs <- combs5, b <- [1..9] ]

