data Move = North | South | East | West
type Pos = (Int, Int)

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (x:xs) p = moves xs (move x p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East
