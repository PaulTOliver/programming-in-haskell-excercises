concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ (Main.concat xs)
