fac :: Int -> Int
fac n
	| n == 0 = 1
	| n >= 1 = n * fac (n - 1)
