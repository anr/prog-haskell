-- Exercises from chapter 8.

-- 1

-- Given:

data Nat = Zero | Succ Nat
         deriving (Eq, Ord, Show, Read)

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

-- We define:

mult :: Nat -> Nat -> Nat
mult Zero     n = Zero
mult (Succ m) n = add (mult m n) n

-- 2

-- Given:

data Tree a = Leaf a | Node (Tree a) a (Tree a)
            deriving (Show, Read)

occurs x (Leaf y)                 = x == y
occurs x (Node l y r) | cmp == EQ = True
                      | cmp == LT = occurs x l
                      | otherwise = occurs x r
  where cmp = compare x y

-- The original 'occurs 'function is inefficient when Tree is a Node
-- and x > y, because it will (unnecessarily) search for x on the
-- left-side Tree.

-- 3

data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving (Show)

leaves :: Tree a -> Int
leaves (Leaf _)   = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree a -> Bool
balanced (Leaf _)   = False
balanced (Node l r) = abs(leaves l - leaves r) < 2

-- 4

bsplit :: [a] -> ([a], [a])
bsplit x = (take n x, drop n x)
  where n = div (length x) 2

balance :: [a] -> Tree a
balance []                = error "Can't create tree from empty list"
balance x  | length x > 1 = Node (balance l) (balance r)
           | otherwise    = Leaf (head x)
  where (l, r) = bsplit x

-- 5

-- Given

data Expr = Val Int | Add Expr Expr
          deriving (Show)

-- We define:

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x)   = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 6

eval :: Expr -> Int
eval e = folde (\x -> x) (\x y -> x + y) e

size :: Expr -> Int
size e = folde (\_ -> 1) (\x y -> x + y) e

-- 7

data Maybe' a = Nothing' | Just' a
              deriving (Show)

instance Eq a => Eq (Maybe' a) where
  Just' x  == Just' y  = x == y
  Nothing' == Nothing' = True
  _        == _        = False

data MyList a = MyList [a]
              deriving (Show)

instance Eq a => Eq (MyList a) where
  MyList []     == MyList []     = True
  MyList (x:xs) == MyList (y:ys) = x == y && MyList xs == MyList ys
  _             == _             = False

-- 8

-- We extend the definitions:

data Prop = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Imply Prop Prop
  | Equiv Prop Prop

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q

-- 9

data Expr = Val Int | Add Expr Expr | Mul Expr Expr

type Cont = [Op]

data Op = EVAL_A Expr | EVAL_M Expr | ADD Int | MUL Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL_A y : c)
eval (Mul x y) c = eval x (EVAL_M y : c)

exec :: Cont -> Int -> Int
exec []             n = n
exec (EVAL_A y : c) n = eval y (ADD n : c)
exec (EVAL_M y : c) n = eval y (MUL n : c)
exec (ADD n : c)    m = exec c (n + m)
exec (MUL n : c)    m = exec c (n * m)

value :: Expr -> Int
value e = eval e []
