import Data.List
import Data.Maybe
import Data.List.Split


words_scores = zip ['A'..'Z'] [1..26]

get_score_char c = fromJust $ lookup c words_scores

get_score_word word = sum $ map get_score_char word






rem_first_last word = tail $ init word 

quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]


read_names file_name = do 
		all_names <- readFile file_name
		let names_split = splitOn "," all_names
		let names = map rem_first_last names_split 
		return names




main = do
		names <- read_names "names.txt"
		let sorted_names = quicksort names
		let scored_names = map get_score_word sorted_names		
		let word_tups = zip [1..] scored_names
		let word_prod = map (\(x,y) -> x*y) word_tups
		return $ sum word_prod
