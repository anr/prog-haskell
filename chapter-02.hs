-- Exercises from chapter 2.

-- 3

n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

-- 4

mylast1 xs = head (reverse xs)
mylast2 xs = xs !! (length xs - 1)

-- 5

myinit1 xs = take (length xs - 1) xs
myinit2 xs = reverse (tail (reverse xs))
