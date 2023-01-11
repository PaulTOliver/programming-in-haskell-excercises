import Data.List

-- parameters
size = 3 :: Int

-- types
data Player = O | B | X deriving (Eq, Ord, Show)
type Grid = [[Player]]

-- utilities
next :: Player -> Player
next O = X
next X = O

full :: Grid -> Bool
full = all (/= B) . concat

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

empty = replicate size (replicate size B) :: Grid

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < (size ^ 2) && (concat g) !! i == B

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then result else [] where
	result = [chop size (xs ++ [p] ++ ys)]
	(xs, B:ys) = splitAt i (concat g)

data Tree a = Node a [Tree a] deriving Show

moves :: Grid -> Player -> [Grid]
moves g p
	| won g = []
	| full g = []
	| otherwise = concat [move g i p | i <- [0 .. ((size ^ 2) - 1)]]

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

gametree_nodes :: Tree Grid -> Int
gametree_nodes (Node x []) = 1
gametree_nodes (Node x ts) = 1 + sum (map gametree_nodes ts)
