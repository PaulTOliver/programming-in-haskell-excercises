-- general version of split returns pairs of empty lists
-- makes countdown problem halt
-- likely at the recursive calls to exprs and results

split' :: [a] -> [([a], [a])]
split' [] = []
split' (x:xs) = ([], x:xs) : ([x], xs) : [(x:ls, rs) | (ls, rs) <- (drop 1 . split') xs]
