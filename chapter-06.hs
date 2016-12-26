-- Exercises from chapter 6.

-- 1

-- A stack overflow happens if a negative argument is passed to the
-- factorial function. Adding a guard to prevent this in the recursive
-- case:

fac :: Int -> Int
fac 0             = 1
fac n | n > 0     = n * fac (n-1)
      | otherwise = error "fac: negative argument"

-- 2

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3

myexp :: Int -> Int -> Int
myexp _ 0 = 1
myexp n m = n * myexp n (m-1)

-- myexp 2 3 = 2 * myexp 2 2
--           = 2 * 2 * myexp 2 1
--           = 2 * 2 * 2 * myexp 2 0
--           = 2 * 2 * 2 * 1
--           = 8

-- 4

euclid :: Int -> Int -> Int
euclid m n | m == n    = m
           | m > n     = euclid (m-n) n
           | otherwise = euclid (n-m) m

-- 5

-- length [1, 2, 3] = 1 + length [2, 3] = 1 + 1 + length [3] = 1 + 1 + 1 + length [] = 1 + 1 + 1 + 0 = 3
-- drop 3 [1, 2, 3, 4, 5] = drop 2 [2, 3, 4, 5] = drop 1 [3, 4, 5] = drop 0 [4, 5] = [4, 5]
-- init [1, 2, 3] = 1 : init [2, 3] = 1 : 2 : init [3] = 1 : 2 : [] = [1, 2]

-- 6

-- a

myand :: [Bool] -> Bool
myand []        = True
myand (False:_) = False
myand (True:xs) = myand xs

-- b

myconcat :: [[a]] -> [a]
myconcat []     = []
myconcat (x:xs) = x ++ myconcat xs

-- c

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n a = [a] ++ myreplicate (n-1) a

-- d

mypos :: [a] -> Int -> a
mypos [] n     = error "mypos: index too large"
mypos (x:_) 0  = x
mypos (_:xs) n = mypos xs (n-1)

-- e

myelem :: Eq a => a -> [a] -> Bool
myelem x []                 = False
myelem x (y:ys) | x == y    = True
                | otherwise = myelem x ys

-- 7

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys                     = ys
merge xs []                     = xs
merge (x:xs) (y:ys) | x < y     = [x] ++ merge xs (y:ys)
                    | otherwise = [y] ++ merge (x:xs) ys

-- 8

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs

-- 9

-- a

mysum :: Num a => [a] -> a
mysum []     = 0
mysum (x:xs) = x + sum xs

-- b

mytake :: Int -> [a] -> [a]
mytake 0 _      = []
mytake n (x:xs) = [x] ++ mytake (n-1) xs

-- c

mylast :: [a] -> a
mylast []     = error "mylast: empty list"
mylast [x]    = x
mylast (x:xs) = mylast xs
