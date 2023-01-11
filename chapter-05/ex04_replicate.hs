replicate :: Int -> a -> [a]
replicate x v = [v | _ <- [0..x]]
