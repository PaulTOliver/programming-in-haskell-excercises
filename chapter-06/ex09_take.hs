take :: Int -> [a] -> [a]
take 0 _ = []
take n (x:xs) = [x] ++ Main.take (n - 1) xs
