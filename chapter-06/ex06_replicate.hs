replicate :: Int -> a -> [a]
replicate n v
	| n == 0 = []
	| otherwise = [v] ++ (Main.replicate (n - 1) v)
