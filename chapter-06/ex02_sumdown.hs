sumdown :: Int -> Int
sumdown n
	| n == 0 = 0
	| n >= 1 = n + sumdown (n - 1)
