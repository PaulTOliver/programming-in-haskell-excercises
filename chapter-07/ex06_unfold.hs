unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)


type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

map :: Eq a => (a -> b) -> [a] -> [b]
map f = unfold (== []) (f . (!! 0)) (drop 1)

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (\_ -> False) f f
