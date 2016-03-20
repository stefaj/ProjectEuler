
every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []

every' n xs = head xs : every n xs

gen n = [n*n,(n*n-1)..((n*n)-((n-1)*(n-1))+1)]

corners 1 = [1]
corners n = take 4 $ [n*n,n*n-(n-1)..]



--n=[1001,1000..1]


sum_corners n = sum [ (sum $ corners k) | k <- [n,(n-2)..1] ]


--sum_corner n = 