seqn [] = return []
seqn (a:as) = do
	x <- a
	xs <- seqn as
	return (x:xs)
