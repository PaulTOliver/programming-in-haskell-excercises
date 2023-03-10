map :: (a -> b) -> [a] -> [b]
map f = foldr (\x y -> (f x) : y) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x y -> if p x then x : y else y) []
