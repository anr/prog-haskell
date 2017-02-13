-- Exercises from chapter 7.

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

