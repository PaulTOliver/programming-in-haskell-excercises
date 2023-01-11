init_v1 xs = [xs !! i | i <- [0..(length xs - 2)]]
init_v2 xs = take (length xs - 1) xs
