pentagon n = (n * (3*n-1)) `div` 2
triangle n = (n * (n+1)) `div` 2
hexagonal n = n * (2*n-1)

pentagon_pos 1 = 1
pentagon_pos pn = (1 + sqrt (1 + 24 * pn)) / 6
trianglePos tn = (-1 + sqrt (1 + 8*tn))/2
hexagonalPos hn = ( 1 + sqrt (1 + 8*hn) ) / 4

--penta_series = [(n * (3*n-1)) `div` 2 | n <- [1..5000]]

is_int n = (fromIntegral $ truncate n) == n


is_pentagon tn = is_int $ pentagon_pos tn
is_triangle tn = is_int $ trianglePos tn
is_hexagon tn = is_int $ hexagonalPos tn



ans = head $ [x | x <- map triangle [286..], is_pentagon $ fromIntegral x, is_hexagon $ fromIntegral x]






