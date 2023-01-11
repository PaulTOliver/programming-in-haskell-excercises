merge :: Ord a => [a] -> [a] -> [a]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs)
	| a < b = [a] ++ (Main.merge as (b:bs))
	| a >= b = [b] ++ (Main.merge bs (a:as))

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where
	n = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort a) (msort b) where
	(a, b) = halve xs
