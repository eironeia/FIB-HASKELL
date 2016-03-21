{-
Feu una funció insert :: [Int] -> Int -> [Int] que, donada una llista ordenada i un element, insereixi ordenadament el nou element a la llista.
Feu una funció isort :: [Int] -> [Int] que implementi l’algorisme d’ordenació per inserció utilitzant la funció anterior.

Feu una funció remove :: [Int] -> Int -> [Int] que, donada una llista i un element x, elimini la primera ocurrència de x de la llista. Podeu assumir que l’element sempre és a la llista.
Feu una funció ssort :: [Int] -> [Int] que implementi l’algorisme d’ordenació per selecció utilitzant la funció anterior.

Feu una funció merge :: [Int] -> [Int] -> [Int] que, donades dues llistes ordenades, les fusioni per obtenir una llista amb tots els seus elements ordenats.
Feu una funció msort :: [Int] -> [Int] que implementi l’algorisme d’ordenació per fusió utilitzant la funció anterior.

Feu una funció qsort :: [Int] -> [Int] que implementi l’algorisme d’ordenació ràpida.
Generalitzeu la funció anterior per fer ara una funció genQsort :: Ord a => [a] -> [a] que ordeni llistes de qualsevol tipus.º

-}

insert :: [Int] -> Int -> [Int]
insert xs y = takeWhile(< y) xs ++ [y] ++ dropWhile(< y) xs

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x


remove :: [Int] -> Int -> [Int]
remove xs y = s ++ drop ((length s)+1) xs
  where s = takeWhile(/= y) xs 

ssort :: [Int] -> [Int]
ssort [] = []
ssort xs = [min] ++ ssort  (remove xs min)
  where min = minimum xs

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = [x] ++ merge xs ([y]++ys)
  | otherwise = [y] ++ merge ([x]++xs) ys

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort as) (msort bs)
  where (as, bs) = splitInHalf xs

splitInHalf :: [a] -> ([a], [a])
splitInHalf [] = ([], [])
splitInHalf [x] = ([x], [])
splitInHalf (x:y:xys) = (x:xs, y:ys)
  where (xs, ys) = splitInHalf xys

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where
        lesser  = [ y | y <- xs, y < p ]
        greater = [ y | y <- xs, y >= p ]



genQsort :: Ord a => [a] -> [a]
genQsort xs = qsort xs