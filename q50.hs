import Control.Monad
import Data.Numbers
import Data.Numbers.Primes
import Data.List
import Control.Parallel.Strategies


short_primes = takeWhile (<1000000) primes
test_primes = takeWhile(<100) primes



consec_prime_sum' [] acc lp n mn = (lp,mn)
consec_prime_sum' (x:xs) acc lp n mn
							| (acc + x) `elem` (x:xs) = consec_prime_sum' xs (acc + x) (acc+x) (n+1) (n+1)
							| otherwise = consec_prime_sum' xs (acc+x) lp (n+1) mn


consec_prime_sum (x:xs) = consec_prime_sum' (x:xs) 0 x 0 0


consec_prime_largest []	lpn = lpn
consec_prime_largest ps@(x:xs) (lp,n) =	let 
											(lp',n') = consec_prime_sum ps
										in
											if n' > n 	then consec_prime_largest xs (lp',n') `using` rseq
														else consec_prime_largest xs (lp,n) `using` rseq






consec_prime_list []	(lp,n) = [(0,lp,n)]
consec_prime_list ps@(x:xs) (lp,n) =	let 
											(lp',n') = (consec_prime_sum ps)
										in
											if n' > n 	then [(x,lp',n')] ++ (consec_prime_list xs (lp',n'))
														else [(x,lp', n')] ++ (consec_prime_list xs (lp,n))



consec_prime_max n = 	let the_primes = takeWhile (<n) primes
						in consec_prime_largest the_primes (0,0)


--main = do 
--		putStrLn $ show ((consec_prime_max 1000000) `using` parList rdeepseq )

 --ans = head $ sortBy (\(a,b) (c,d) -> compare d b) $ consec_prime_sum test_primes 0 0