import Data.Numbers
import Data.Numbers.Primes
import Data.List
import qualified Data.Set as S


same_digits n1 n2 = (sort $ show n1) == (sort $ show n2)







ans = head $ [x | x <- [1..], (same_digits x (x*2)) && (same_digits x (x*3)) && (same_digits x (x*4)) && (same_digits x (x*5)) && (same_digits x (x*6))]