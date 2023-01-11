bools = [True, True, True]

nums = [[1,2,3], [4,5,6]] :: [[Int]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f a = f a
