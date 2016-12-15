-- Exercises from chapter 3.

-- 1

['a', 'b', 'c'] :: [Char]

('a', 'b', 'c') :: (Char, Char, Char)

[(False, '0'), (True, '1')] :: [(Bool, Char)]

([False, True], ['0', '1']) :: ([Bool], [Char])

[tail, init, reverse] :: [[a] -> [a]]

-- 2

[False, True]

[[1, 2], [3, 4]]

add x y z = x + y + z

copy x = (x, x)

apply f a = f a

-- 3

second :: [a] -> a

swap :: (a, b) -> (b, a)

pair :: a -> b -> (a, b)

double :: Num a => a -> a

palindrome :: Eq a => [a] -> Bool

twice :: (a -> a) -> a -> a
