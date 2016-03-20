--import Data.List
--import Data.Maybe
--import Data.Set as S
--import Data.List.Split


--pentagon :: Int -> Int
pentagon n = (n * (3*n-1)) `div` 2
triangle n = (n * (n+1)) `div` 2
hexagonal n = n * (2*n-1)

pentagon_pos 1 = 1
pentagon_pos pn = (1 + sqrt (1 + 24 * pn)) / 6
trianglePos tn = (-1 + sqrt (1 + 8*tn))/2
hexagonalPos hn = ( 1 + sqrt (1 + 8*hn) ) / 4

--penta_series = [(n * (3*n-1)) `div` 2 | n <- [1..5000]]

is_int n = (fromIntegral $ truncate n) == n


is_pentagon = is_int . pentagon_pos
is_triangle = is_int . trianglePos
is_hexagon = is_int . hexagonalPos



ans = head $ [x | x <- map triangle [1..], is_pentagon x, is_hexagon x]