-- now this one was harder than anticipated!

import System.IO

read_char :: IO Char
read_char = do
	hSetEcho stdin False
	x <- getChar
	hSetEcho stdin True

	case x of
		'\DEL' -> putStr "\b \b"
		c -> putChar c

	return x

read_stream :: IO String
read_stream = do
	x <- read_char

	if x == '\n' then do
		return []
	else do
		xs <- read_stream
		return (x:xs)

safe_init :: String -> String
safe_init [] = []
safe_init xs = init xs

process :: String -> String
process [] = []
process xs
	| not ('\DEL' `elem` xs) = xs
	| otherwise = process (h ++ t) where
		h = safe_init (takeWhile (/= '\DEL') xs)
		t = tail (dropWhile (/= '\DEL') xs)

read_line :: IO String
read_line = do
	xs <- read_stream
	return (process xs)
