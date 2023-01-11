(^) :: Int -> Int -> Int
(^) n e
	| e == 0 = 1
	| e >= 1 = n * (n Main.^ (e - 1))
