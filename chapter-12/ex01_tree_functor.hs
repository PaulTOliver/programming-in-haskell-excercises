data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
	-- fmap :: (a -> b) -> Tree a -> Tree b
	fmap f (Leaf x) = Leaf (f x)
	fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
