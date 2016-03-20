sum_squares n = sum [i^2 | i <- [1..n]]

squared_sum n = (sum [1..n])^2

diff n = (squared_sum n) - (sum_squares n) 