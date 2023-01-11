adder :: IO ()
adder = do
	putStr "How many numbers? "
	nums <- getLine
	nseq <- sequence [getLine | _ <- [1 .. (read nums)]]
	putStrLn (show (sum (map read nseq)))
