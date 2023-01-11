type Board = [Int]

b1 :: Board
b1 = [5, 4, 3, 2, 1]

b2 :: Board
b2 = [3, 5, 2, 4, 1, 3, 0, 2]

b3 :: Board
b3 = [1, 3, 5]

put_board :: Board -> IO ()
put_board b = sequence_ [put_line n x | (x, n) <- zip b [1 ..]] where
	put_line n x = putStrLn (show n ++ ": " ++ concat (replicate x "* "))
