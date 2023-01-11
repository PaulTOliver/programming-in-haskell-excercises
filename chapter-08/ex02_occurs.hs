data Tree a = Leaf a | Node (Tree a) a (Tree a)

flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

highest_in_tree :: Ord a => Tree a -> a
highest_in_tree (Leaf a) = a
highest_in_tree (Node l x r) = hl `max` x `max` hr where
	hl = highest_in_tree l
	hr = highest_in_tree r

is_search_tree :: Ord a => Tree a -> Bool
is_search_tree (Leaf a) = True
is_search_tree (Node l x r) = hl < x && x < hr where
	hl = highest_in_tree l
	hr = highest_in_tree r

stree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

--occurs :: Ord a => a -> Tree a -> Bool
--occurs x (Leaf y) = x == y
--occurs x (Node l y r)
--	| x == y = True
--	| x < y = occurs x l
--	| otherwise = occurs x r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = compare x y == EQ
occurs x (Node l y r) = case compare x y of
	EQ -> True
	LT -> occurs x l
	GT -> occurs x r
