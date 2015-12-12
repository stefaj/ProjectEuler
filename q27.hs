
import Data.List
import Data.Maybe
import Data.List.Split
import Data.Ord


isPrime :: Int -> Bool
isPrime k
		| k <= 0 = False 
		|otherwise = null [ x | x <- [2..(truncate $ sqrt $ fromIntegral k)], k `mod` x  == 0]


--qn :: Int -> Int -> Int -> Int
qn a b n = n*n + a*n + b



--max_consec [] acc = acc
--max_consec (x:xs) acc = if isPrime x then max_consec xs (acc + 1) 
--						else max_consec xs 0


consecs xs = map isPrime xs
max_consec' xs = splitOn [False] (consecs xs)
max_consec xs = maximum (map length (max_consec' xs))





n = [1..100]

max_primes_quad (a,b) = map (qn a b) n
max_consec_primes = map (\(a,b) -> (a * b, max_consec (max_primes_quad (a,b))  )) [(a,b) | a <- [-100..100], b <- [-1000..1000], a+b > 0] 


quads = [(a,b,n) | a <- [-100..100], b <- [-1000..1000]]

test_consecs = map (\(a,b,n) -> (a*b, max_consec n)) quads


ans = maximumBy (comparing snd) test_consecs


--seqs = [qn a b n, (a,b) <- quads,]

--test_seqs = [ ((a*b), qn a b n) | (n,a,b) <- quads]
