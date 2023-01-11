import Data.Char
import Data.List
import System.IO
import System.Random

-- parameters
size = 3 :: Int
prune_depth = 9 :: Int

-- types
data Player = O | B | X deriving (Eq, Ord, Show)
type Grid = [[Player]]

-- utilities
next_p :: Player -> Player
next_p O = X
next_p X = O

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X where
	os = length (filter (== O) ps)
	xs = length (filter (== X) ps)
	ps = concat g

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. (size - 1)]]

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias) where
	line = all (== p)
	rows = g
	cols = transpose g
	dias = [diag g, diag (map reverse g)]

won :: Grid -> Bool
won g = wins O g || wins X g

-- std. variables
empty = replicate size (replicate size B) :: Grid

-- display
show_player :: Player -> [String]
show_player O = ["     ", "  O  ", "     "]
show_player B = ["     ", "     ", "     "]
show_player X = ["     ", "  X  ", "     "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

show_row :: [Player] -> [String]
show_row = beside . interleave bar . map show_player where
	beside = foldr1 (zipWith (++))
	bar = replicate 3 "|"

put_grid :: Grid -> IO ()
put_grid = putStrLn . unlines . concat . interleave bar . map show_row where
	bar = [replicate ((size * 6) - 1) '-']

-- play
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < (size ^ 2) && (concat g) !! i == B

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then result else [] where
	result = [chop size (xs ++ [p] ++ ys)]
	(xs, B:ys) = splitAt i (concat g)

get_nat :: String -> IO Int
get_nat prompt = do
	putStr prompt
	xs <- getLine

	if xs /= [] && all isDigit xs then
		return (read xs)
	else do
		putStrLn "ERROR: Invalid number."
		get_nat prompt

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

run :: Grid -> Player -> IO ()
run g p = do
	cls
	goto (1, 1)
	put_grid g
	run' g p

run' :: Grid -> Player -> IO ()
run' g p
	| wins O g = putStrLn "Player O wins!\n"
	| wins X g = putStrLn "Player X wins!\n"
	| full g = putStrLn "It's a draw!\n"
	| otherwise = do
		i <- get_nat (prompt p)

		case move g i p of
			[] -> do
				putStrLn "ERROR: Invalid move."
				run' g p
			[g'] -> run g' (next_p p)

tictactoe :: IO ()
tictactoe = run empty O

-- single player
data Tree a = Node a [Tree a] deriving Show

moves :: Grid -> Player -> [Grid]
moves g p
	| won g = []
	| full g = []
	| otherwise = concat [move g i p | i <- [0 .. ((size ^ 2) - 1)]]

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next_p p) | g' <- moves g p]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
	| wins O g = Node (g, O) []
	| wins X g = Node (g, X) []
	| otherwise = Node (g, B) []
minimax (Node g ts)
	| turn g == O = Node (g, minimum ps) ts'
	| turn g == X = Node (g, maximum ps) ts'
		where
			ps = [p | Node (_, p) _ <- ts']
			ts' = map minimax ts

randmove :: Grid -> Player -> IO Grid
randmove g p = do
	ridx <- randomRIO (0, best_count - 1)
	return (best_moves !! ridx)
		where
			best_count = length best_moves
			best_moves = [g' | Node (g', p') _ <- ts, p' == best]
			Node (_, best) ts = minimax tree
			tree = prune prune_depth (gametree g p)

play :: Grid -> Player -> IO ()
play g p = do
	cls
	goto (1, 1)
	put_grid g
	play' g p

play' :: Grid -> Player -> IO ()
play' g p
	| wins O g = putStrLn "Player O wins!\n"
	| wins X g = putStrLn "Player X wins!\n"
	| full g = putStrLn "It's a draw!\n"
	| p == O = do
		i <- get_nat (prompt p)

		case move g i p of
			[] -> do
				putStrLn "ERROR: Invalid move."
				play' g p
			[g'] -> play g' (next_p p)
	| p == X = do
		putStr "Player X is thinking... "
		rmove <- randmove g p
		(play $! rmove) (next_p p)

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	play empty O
