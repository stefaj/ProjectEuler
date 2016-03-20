import Data.List
import Data.Maybe
import Data.List.Split



word_lookup = zip ['A'..'Z'] [1..]

word_value word = sum $ map (\c -> fromJust $ lookup c word_lookup) word

-- Triangle stuff

triangle n = (n * (n+1)) `div` 2

is_int n = (fromIntegral $ truncate n) == n

trianglePos tn = (-1 + sqrt (1 + 8*tn))/2

is_triangle tn = is_int $ trianglePos tn


rem_first_last word = tail $ init word 

read_names file_name = do 
		all_names <- readFile file_name
		let names_split = splitOn "," all_names
		let names = map rem_first_last names_split 
		return names

main = do
		names <- read_names "words.txt"
		let triangle_words = filter (is_triangle . word_value) names
		return triangle_words
