coins = [1,2,5,10,20,50,100,200]
coinMaxCombo = map (\x -> 200 `div` x) coins
coin_vals = zip coins coinMaxCombo


--coins = [(1,200),(2,100),(5,40),(10,20),(20,10),(50,4),(100,2),(200,1)]


effective (c,n) = c*n

is_valid xs = sum xs == 200




coin_func (c,n) = [effective (c,n) | n <- [0..n]]

ec = map coin_func coin_vals

combos = [ [a,b,c,d,e,f,g,h] | a <- ec !! 0, b <- ec !! 1, c <- ec !! 2, d <- ec !! 3,
							 e <- ec !! 4, f <- ec !! 5, g <- ec !! 6, h <- ec !! 7,
							 a+b+c+d+e+f+g+h == 200]



withcoins 1 x = [[x]]
withcoins n x = concatMap addCoin [0 .. x `div` coins!!(n-1)]
  where addCoin k = map (++[k]) (withcoins (n-1) (x - k*coins!!(n-1)) )

effective_combo = map (\xs -> zipWith (*) coins xs) $ withcoins (length coins) 200
--combos2 = zipWith (*) coins $ withcoins (length coins) 200 


ans2 = length $ effective_combo