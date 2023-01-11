(!!) :: [a] -> Int -> a
(!!) (x:xs) i
	| i == 0 = x
	| i >= 1 = xs Main.!! (i - 1)
