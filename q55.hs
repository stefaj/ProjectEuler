import Data.Numbers
import Data.Numbers.Primes
import Data.List
import qualified Data.Set as S

isPalindrome n = (reverse $ show n) == show n

addReverse n = n + (read $ reverse $ show n)

isLychrel n 50 = (True, 50)
isLychrel n it = let n' = addReverse n in 
 if (isPalindrome n' )
	then (False, it)
	else isLychrel n' (it + 1)
-- isLychrel 106771 = (True, 50)
-- isLychrel 349 1 = (False, 3)


isLychrel' n = fst $ isLychrel n 1



ans = length $ filter isLychrel' [1..10000] 