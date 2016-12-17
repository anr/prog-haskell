-- Exercises from chapter 4.

-- 1

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- 2

thirda :: [a] -> a
thirda xs = head (tail (tail xs))

thirdb :: [a] -> a
thirdb xs = xs !! 2

thirdc :: [a] -> a
thirdc (_:_:x:_) = x

-- 3

safetaila :: [a] -> [a]
safetaila xs = if null xs then [] else tail xs

safetailb :: [a] -> [a]
safetailb xs | null xs   = []
             | otherwise = tail xs

safetailc :: [a] -> [a]
safetailc (_:xs) = xs
safetailc _      = []

-- 4

or1 :: Bool -> Bool -> Bool
or1 True  True  = True
or1 True  False = True
or1 False True  = True
or1 False False = False

or2 :: Bool -> Bool -> Bool
or2 False False = False
or2 _     _     = True

or3 :: Bool -> Bool -> Bool
or3 True  _ = True
or3 False x = x

or4 :: Bool -> Bool -> Bool
or4 x y | x /= y    = True
        | otherwise = x

-- 5

and5 :: Bool -> Bool -> Bool
and5 x y = if x == True then
             if y == True then True
             else False
           else False

-- 6

and6 :: Bool -> Bool -> Bool
and6 x y = if x == True then y else False

-- 7

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

-- 8

luhnDouble :: Int -> Int
luhnDouble d | 2 * d > 9 = 2 * d - 9
             | otherwise = 2 * d

luhn :: Int -> Int -> Int -> Int -> Bool
luhn d4 d3 d2 d1 = if sum `mod` 10 == 0 then True else False
                   where sum = d1 + (luhnDouble d2) + d3 + (luhnDouble d4)
