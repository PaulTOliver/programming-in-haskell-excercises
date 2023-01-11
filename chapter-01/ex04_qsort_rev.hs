qsort_rev [] = []
qsort_rev (x:xs) = qsort_rev larger ++ [x] ++ qsort_rev smaller where
	larger = [a | a <- xs, a >= x]
	smaller = [b | b <- xs, b < x]
