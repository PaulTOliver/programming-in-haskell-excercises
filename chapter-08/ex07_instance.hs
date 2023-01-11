data Maybe a = Nothing | Just a

instance Eq a => Eq (Main.Maybe a) where
	Main.Nothing == Main.Nothing = True
	Main.Just x == Main.Just y = x == y
	_ == _ = False

-- ghci complains of the following re-declaration
--instance Eq a => Eq [a] where
--	x == y = length x == length y && list_eq where
--		list_eq = pair_eq (zip x y)
--		pair_eq = foldr (\(x, y) -> (&& (x == y))) True
--	_ == _ = False
