import Data.List
import Data.Ord

ans =  fst $ maximumBy (comparing snd) [(n,remainders n 10 []) | n <- [1..999]]

remainders d 0 rs = 0
remainders d r rs = let r' = r `mod` d
					in case elemIndex r' rs of
                    	Just i  -> i + 1
                        Nothing -> remainders d (10*r') (r':rs)