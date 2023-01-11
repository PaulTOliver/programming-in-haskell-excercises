safetail_v1 xs = if null xs then xs else tail xs

safetail_v2 xs
	| null xs = xs
	| otherwise = tail xs

safetail_v3 [] = []
safetail_v3 (x:xs) = xs
