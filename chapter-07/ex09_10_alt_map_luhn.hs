alt_map :: (a -> b) -> (a -> b) -> [a] -> [b]
alt_map _ _ [] = []
alt_map f1 f2 (x:xs) = f1 x : alt_map f2 f1 xs

luhn_double :: Int -> Int
luhn_double a
	| a > 4 = a * 2 - 9
	| otherwise = a * 2

luhn :: [Int] -> Bool
luhn xs = ((sum (alt_map luhn_double id xs)) `mod` 10) == 0
