type Board = [Int]

b1 :: Board
b1 = [5, 4, 3, 2, 1]

b2 :: Board
b2 = [3, 5, 2, 4, 1, 3, 0, 2]

b3 :: Board
b3 = [1, 3, 5]

put_lines :: Int -> [Int] -> IO ()
put_lines _ [] = return ()
put_lines n (x:xs) = do
	putStrLn (show n ++ ": " ++ concat (replicate x "* "))
	put_lines (n + 1) xs

put_board :: Board -> IO ()
put_board = put_lines 1
