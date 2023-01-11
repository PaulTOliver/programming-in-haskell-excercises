(||) :: Bool -> Bool -> Bool
b || c
	| b /= c = True
	| otherwise = b
