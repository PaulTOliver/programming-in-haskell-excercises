curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x -> (\y -> f (x, y))

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \x -> f (fst x) (snd x)

-- we can test the above functions using the functions below as parameters
sum_pair :: Num a => (a, a) -> a
sum_pair (x, y) = x + y

sum_curry :: Num a => a -> a -> a
sum_curry x y = x + y
