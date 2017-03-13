-- Exercises from chapter 7.

import Data.Char

-- 1

lc :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lc f p = map f . filter p

-- 2

-- a

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

-- b

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

-- c

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                 = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []

-- d

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []                 = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x : xs

-- 3

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

-- 4

dec2int :: [Int] -> Int
dec2int = foldl (\y z -> 10 * y + z) 0

-- 5

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

-- 6

-- Given unfold:

unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)

map2 :: (a -> b) -> [a] -> [b]
map2 f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f

-- 7

-- Given:

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- we add:

add_parity :: [Bit] -> [Bit]
add_parity bits = bits ++ [mod (sum bits) 2]

check_parity :: [Bit] -> [Bit]
check_parity bits | add_parity octet == bits = octet
                  | otherwise                = error "parity error"
  where octet = take 8 bits

-- and redefine:

encode :: String -> [Bit]
encode = concat . map (add_parity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . check_parity) . chop9

-- 8

-- Redefining transmit as

transmit' = decode . tail . encode

-- generates a parity error.

-- 9

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []     = []
altMap f g (x:xs) = f x : altMap g f xs

-- 10

luhn :: [Int] -> Bool
luhn xs = mod ((sum . map f . altMap id (*2) . reverse) xs) 10 == 0
  where f = \x -> if x > 9 then x - 9 else x
