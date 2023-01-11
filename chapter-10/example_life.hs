-- screen ops
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

write_at :: Pos -> String -> IO ()
write_at p xs = do
	goto p
	putStr xs

-- parameters
width :: Int
width = 50

height :: Int
height = 25

-- main logic
type Board = [Pos]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

show_cells :: Board -> IO ()
show_cells b = sequence_ [write_at p "0" | p <- b]

is_alive :: Board -> Pos -> Bool
is_alive b p = p `elem` b

is_empty :: Board -> Pos -> Bool
is_empty b = not . is_alive b

wrap :: Pos -> Pos
wrap (x, y) = (x', y') where
	x' = ((x - 1) `mod` width) + 1
	y' = ((y - 1) `mod` height) + 1

neighbors :: Pos -> [Pos]
neighbors (x, y) = map wrap [bl, bm, br, lm, rm, tl, tm, tr] where
	bl = (x - 1, y - 1)
	bm = (x, y - 1)
	br = (x + 1, y - 1)
	lm = (x - 1, y)
	rm = (x + 1, y)
	tl = (x - 1, y + 1)
	tm = (x, y + 1)
	tr = (x + 1, y + 1)

live_neighbors :: Board -> Pos -> Int
live_neighbors b = length . filter (is_alive b) . neighbors

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (live_neighbors b p) [2, 3]]

-- Using universal property of fold described in 'A tutorial on the
-- universality and expressiveness of fold', I was able to improve the book's
-- proposed version of rmdups. This one is much nicer. :) You can search for
-- this paper (also by G. Hutton) online.
rmdups :: Eq a => [a] -> [a]
rmdups = foldr (\x xs -> x : filter (/= x) xs) []

births :: Board -> [Pos]
births b = [p | p <- ns, is_empty b p, live_neighbors b p == 3] where
	ns = (rmdups . concat. map neighbors) b

next_generation :: Board -> Board
next_generation b = survivors b ++ births b

-- main loop
-- For some reason, running this in ghci consumes tons of RAM. Compiling makes
-- the program run much lighter.
wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1 .. n]]

life :: Board -> IO ()
life b = do
	cls
	show_cells b
	wait 50000000
	life (next_generation b)

main :: IO ()
main = life glider
