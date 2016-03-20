import Data.List
import Data.Ord

is_co_prime a b = gcd a b == 1
triangles2 p = [(a,b,c) | 	n <- [1..floor (sqrt $ fromIntegral p)], m <- [n+1..floor (sqrt $ fromIntegral p)], k <- [1..100], let a = (k * (m^2 - n^2)),	let b = (k * (2*m*n)), let c = (k * (m^2 + n^2)), m > n, a+b+c==p, is_co_prime m n, (m-n) `mod` 2 == 1  ]
ans = maximumBy (comparing snd) $ [(p, length $ triangles2 p) | p <- [1..1000]]cabal