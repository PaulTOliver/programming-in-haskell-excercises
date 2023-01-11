and :: [Bool] -> Bool
and [x] = x
and (x:xs) = x && (Main.and xs)
