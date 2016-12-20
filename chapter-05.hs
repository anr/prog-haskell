-- Exercises from chapter 5.

import Data.Char

-- 1

squaresSum = sum [x^2 | x <- [1..100]]

-- 2

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 3

square :: Int -> [(Int, Int)]
square n = [(x , y) | (x, y) <- grid n n, x /= y]

-- 4

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- 5

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

-- 7

alternateForm = concat [[(x, y), (x, y + 1)] | (x, y) <- [(x, 3) | x <- [1, 2]]]

-- 8

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- 9

scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct xs ys = sum [x * ys !! i | (x, i) <- zip xs [0..]]

-- 10

let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | otherwise = ord c - ord 'A'

int2let :: Bool -> Int -> Char
int2let lower n | lower     = chr (ord 'a' + n)
                | otherwise = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isAlpha c = int2let (isLower c) ((let2int c + n) `mod` 26)
          | otherwise = c
