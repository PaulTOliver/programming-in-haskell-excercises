import Data.Char

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1..] board] where
	update r n = if r == row then (n - num) else n

-- IO ops
put_row :: Int -> Int -> IO ()
put_row row num = putStrLn (show row ++ ": " ++ concat (replicate num "* "))

put_board :: Board -> IO ()
put_board [a, b, c, d, e] = do
	put_row 1 a
	put_row 2 b
	put_row 3 c
	put_row 4 d
	put_row 5 e

newline :: IO ()
newline = putChar '\n'

get_digit :: String -> IO Int
get_digit prompt = do
	putStr prompt
	x <- getChar

	if isDigit x then do
		newline
		return (digitToInt x)
	else do
		putStrLn " ERROR: Invalid digit"
		get_digit prompt

play :: Board -> Int -> IO ()
play board player = do
	newline
	put_board board

	if finished board then do
		newline
		putStrLn ("Player " ++ (show . next) player ++ " wins!")
	else do
		newline
		putStrLn ("Player " ++ show player)
		row <- get_digit "Enter a row number: "
		num <- get_digit "Stars to remove: "

		if valid board row num then
			play (move board row num) (next player)
		else do
			newline
			putStrLn "ERROR: Invalid move. Please re-try."
			play board player

nim :: IO ()
nim = play initial 1
