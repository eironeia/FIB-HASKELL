
-- 1.1

mconcat2::[[a]]->[a]
mconcat2 xs = [ x | w <- xs,  x <- w] 

-- 1.2
mconcat3:: [[[a]]] -> [a]
mconcat3 xs = [x | w <- xs, y <- w, x <- y]

-- 2

--				f 			elem	ll1		ll2  res
fold2r :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
fold2r f seed (a:as) (b:bs) = f a b (fold2r f seed as bs)
fold2r _ seed _ _ = seed

-- 3

mix:: [a] -> [a] -> [a]
mix [] ys = ys
mix xs [] = xs
mix (x:xs) (y:ys) = [x]++[y]++(mix xs ys) 

lmix:: [Int] -> [a] -> [a]
lmix [] ys = ys
lmix (x:xs) ys =(lmix xs z)
    where z = (mix (take x ys) (drop x ys))

-- 4

pascal :: [[Integer]]
pascal = iterate (tail.(scanl (+) 0)) [1..]

dpascal :: Int -> [Integer]
dpascal n = pascal !!(n - 1)


-- 5
--Node 2 (Node 4 (Node 7 Empty Empty) (Node 2 Empty Empty)) (Node 8 (Node 1 Empty
--Empty) Empty)

data BTree a = Node a (BTree a) (BTree a) | Empty deriving (Show)

construeix :: [[a]] -> Int -> BTree a 
construeix [] x = Empty
construeix (x:xs) pos
    | pos < (length x) =  Node (x!!pos) (construeix xs (2*pos)) (construeix xs (2*pos + 1))
    | otherwise = Empty 
    
buildTreeF :: [[a]] -> BTree a
buildTreeF [] = Empty
buildTreeF (x:xs) = Node (x!!0) (construeix xs 0) (construeix xs 1)


-- 6

--6.1

class Lit a where
    unary:: a -> a
    binary::  a -> a -> a
    list:: [a] -> a

-- 6.2

data Expr a = Val a | Unary (Expr a) | Binary (Expr a) (Expr a) | List [Expr a] deriving (Show)

ex1 :: Expr Int
ex1 = Unary (Binary (List [Val 3, Unary (Val 2)]) (Val 8))

-- 6.3

eval:: (Lit a) => Expr a -> a
eval (Val a) = a
eval (Unary ex) = unary (eval ex)
eval (Binary ex1 ex2) = binary (eval ex1) (eval ex2)
eval (List []) = list []
eval (List (x:xs)) = list ([eval x]++[eval (List xs)])


instance Lit Int where
    unary x = (-x)
    binary x y = (x+y)
    list xs = sum xs