luhn_double :: Int -> Int
luhn_double a
	| a > 4 = a * 2 - 9
	| otherwise = a * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum `mod` 10 == 0 where
	sum = luhn_double a + b + luhn_double c + d
