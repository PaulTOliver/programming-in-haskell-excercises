f :: Integral a => a -> a
f = (+ 1)

p :: Integral a => a -> Bool
p = even

xs = [1..7] ++ [16..23]

-- these 2 expressions should be equivalent
l1 = [f x | x <- xs, p x]
l2 = (map f . filter p) xs
