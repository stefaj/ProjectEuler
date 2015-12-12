import Data.Char
import Data.Bits
import Data.List
import Data.Ord

toStr xs = concat $ map (\x -> (chr x):"") xs


frequency s = map (\x->([head x], length x)) . group . sort $ s
frequencySorted s = reverse . sortBy (comparing snd) $ frequency s

every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []


every3 n xs = every 3 (drop n  ([0,0] ++ xs))



mapXor s1 s2 = map (\(x,y) -> x `xor` y) $ zip s1 s2

cycleKey str key = let key' = take (length str) (cycle key) in
	mapXor str key'



testKey key = do
	dat <- readFile "p059_cipher.txt"
	let nums = read ("[" ++ dat ++ "]") :: [Int]
	let dec = cycleKey nums key
	putStrLn $ toStr dec


getValue str = sum $ map ord str

main = do
	dat <- readFile "p059_cipher.txt"
	let nums = read ("[" ++ dat ++ "]") :: [Int]
	let f1 = frequencySorted $ every3 0 nums
	let f2 = frequencySorted $ every3 1 nums
	let f3 = frequencySorted $ every3 2 nums
	putStrLn $ show f3

	let dec = cycleKey nums [103,111,100]
	putStrLn $ toStr dec
	putStrLn $ show $ getValue (toStr dec)
	--putStrLn $ show $ every3 0 nums
