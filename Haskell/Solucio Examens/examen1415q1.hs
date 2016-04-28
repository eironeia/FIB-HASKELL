
-- Problema 1

-- Entrada: prefsufs [1,2,3,4] 
-- Sortida: [[1],[1,2],[1,2,3],[1,2,3,4],[2,3,4],[3,4],[4]]

leftList:: [a] -> [[a]]
leftList xs = map (\x -> take x xs) [1..((length xs)-1)]

rightList:: [a] -> [[a]]
rightList xs = map (\x -> drop x xs) [1..((length xs)-1)]

prefsufs::[a]->[[a]]
prefsufs xs = (leftList xs)++[xs]++(rightList xs)

-- Problema 2

-- Entrada:
-- Sortida:	

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x0 = until (\x -> x == f x) f x0

-- Problema 3


-- 3.1

data Polynomial a = Polynomial [a] deriving (Show)

instance (Num a, Ord a) => Eq (Polynomial a) where
    Polynomial a == Polynomial b = (take y a) == (take y b)
        where y = min (length a) (length b) 

-- 3.2

{-
instance (Num a, Ord a) => Num (Polynomial a) where
    (Polynomial a) + (Polynomial b) = Polynomial (zipWith (+) a b)
    (Polynomial a) * (Polynomial b) = Polynomial (zipWith (*) a b)
    abs (Polynomial a) = Polynomial (map (\x -> abs x) a)
    --signum (Polynomial []) = Polynomial 0
    --signum (Polynomial a) = signum (a!!0)
    fromInteger x = Polynomial [(fromInteger x)]

-}
-- 4

--(ALeaf 5) (Nand [ (Nor [ ALeaf 5, (Nand [(Nor [ALeaf 2, ALeaf 1]), OLeaf 4 ])]), (Nor [ ALeaf 3, ALeaf 4 ]) ])

data Disj a = OLeaf a | Nor [AndOr a] deriving (Show)

data AndOr a = ALeaf a | Nand [Disj a] deriving (Show)


evalAux:: (a -> Bool) -> (Disj a) -> Bool
evalAux p (OLeaf a) = p a
evalAux p (Nor xs) = or $ map (\x -> (eval p x)) xs

eval :: (a -> Bool) -> (AndOr a) -> Bool
eval p (ALeaf a) = p a
eval p (Nand xs) = and $ map (\x -> (evalAux p x)) xs




















