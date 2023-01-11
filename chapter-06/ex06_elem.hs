elem :: Eq a => a -> [a] -> Bool
elem a (x:xs)
	| a == x = True
	| null xs = False
	| otherwise = Main.elem a xs
