remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove n (x:xs) = if n == x then xs else x : (remove n xs)

is_choice :: Eq a => [a] -> [a] -> Bool
is_choice [] _ = True
is_choice (x:xs) ys = if diff then False else (is_choice xs ys') where
	diff = length ys == length ys'
	ys' = remove x ys
