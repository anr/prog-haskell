-- Exercises from chapter 1.

-- 3

myproduct []     = 1
myproduct (n:ns) = n * myproduct ns

-- 4

rqsort []     = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
  where
    larger  = [a | a <- xs, a > x]
    smaller = [b | b <- xs, b <= x]

-- 5

-- The numbers in the output will be distinct (repetitions will be
-- discarded).
