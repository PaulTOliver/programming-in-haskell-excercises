data Tree a = Leaf a | Node (Tree a) (Tree a)
	deriving Show

-- this naive approach requires multiple traversals and is slow
leafs :: Tree a -> Int
leafs (Leaf _) = 1
leafs (Node l r) = leafs l + leafs r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = d1 && bl && br where
	d1 = abs (leafs l - leafs r) <= 1
	br = balanced r
	bl = balanced l

-- this approach traverses the tree once and is faster
balance_checker :: Tree a -> (Bool, Int)
balance_checker (Leaf _) = (True, 1)
balance_checker (Node l r) = (b, c) where
	b = lb && rb && abs (lc - rc) <= 1
	c = lc + rc
	(lb, lc) = balance_checker l
	(rb, rc) = balance_checker r

balanced_v2 :: Tree a -> Bool
balanced_v2 = fst . balance_checker

split :: [a] -> ([a], [a])
split xs = (l, r) where
	l = take h xs
	r = drop h xs
	h = (length xs) `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance l) (balance r) where
	(l, r) = split xs
