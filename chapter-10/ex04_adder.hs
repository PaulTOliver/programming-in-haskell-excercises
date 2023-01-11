add_nums :: Int -> Int -> IO ()
add_nums 0 x = putStrLn (show x)
add_nums c x = do
	nsum <- getLine
	add_nums (c - 1) (x + (read nsum))

adder :: IO ()
adder = do
	putStr "How many numbers? "
	nums <- getLine
	add_nums (read nums) 0
