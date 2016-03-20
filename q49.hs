{-# LANGUAGE NoMonomorphismRestriction #-}


import Control.Monad
import Data.Numbers
import Data.Numbers.Primes
import Data.List


short_primes = do
				x <- takeWhile (<10000) primes
				guard $ x > 999
				return x


ans = do
		a <- short_primes
		b <- dropWhile (>a) short_primes
		guard (b > a)
		let c = 2*b-a
		guard $ (sort $ show a) == (sort $ show b)
		guard $ (sort $ show b) == (sort $ show c)
		guard $ c `elem` short_primes
		return (a,b,c) 