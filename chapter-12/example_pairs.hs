pairs_comp :: [a] -> [b] -> [(a, b)]
pairs_comp xs ys = [(x, y) | x <- xs, y <- ys]

pairs_do :: [a] -> [b] -> [(a, b)]
pairs_do xs ys = do
	x <- xs
	y <- ys
	return (x, y)

pairs_bind :: [a] -> [b] -> [(a, b)]
pairs_bind xs ys = xs >>= \x -> ys >>= \y -> [(x, y)]
