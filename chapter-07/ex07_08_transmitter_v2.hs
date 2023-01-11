import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

parity_bit :: [Bit] -> Bit
parity_bit = ((`mod` 2) . length . (filter (== 1)))

make9 :: [Bit] -> [Bit]
make9 bits = parity_bit bits : (take 8 (bits ++ repeat 0))

encode :: String -> [Bit]
encode = concat . map (make9 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

check_bit :: [Bit] -> [Bit]
check_bit bit = if check then t_bit else m_err where
	check = (parity_bit t_bit) == (bit !! 0)
	t_bit = tail bit
	m_err = error "Parity bit error."

decode :: [Bit] -> String
decode = map (chr . bin2int . check_bit) . chop9

channel :: [Bit] -> [Bit]
channel = id

faulty_channel :: [Bit] -> [Bit]
faulty_channel = tail

transmit :: ([Bit] -> [Bit]) -> String -> String
transmit c = decode . c . encode
