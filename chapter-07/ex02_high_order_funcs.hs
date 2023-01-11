-- there seems to be an error on the second edition of the book regarding the
-- data type of functions proposed on exercises 2a and 2b

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
	| p x = x : Main.takeWhile p xs
	| otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs)
	| p x = Main.dropWhile p xs
	| otherwise = x : xs
