l1 = [(x, y) | x <- [1, 2], y <- [3, 4]]
l2 = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]
