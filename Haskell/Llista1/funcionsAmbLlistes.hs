{-
Feu una funció myLength :: [Int] -> Int que, donada una llista d’enters, calculi la seva llargada.
Feu una funció myMaximum :: [Int] -> Int que, donada una llista d’enters no buida, calculi el seu màxim.
Feu una funció average :: [Int] -> Float que, donada una llista d’enters no buida, calculi la seva mitjana.
Feu una funció buildPalindrome :: [Int] -> [Int] que, donada una llista, retorni el palíndrom que comença amb la llista invertida.
Feu una funció remove :: [Int] -> [Int] -> [Int] que donada una llista d’enters x i una llista d’enters y, retorna la llista x havent eliminat totes les ocurrències dels elements en y.
Feu una funció flatten :: [[Int]] -> [Int] que aplana una llista de llistes produint una llista d’elements.
Feu una funció oddsNevens :: [Int] -> ([Int],[Int]) que, donada una llista d’enters, retorni dues llistes, una que conté els parells i una que conté els senars, en el mateix ordre relatiu que a l’original.
Feu una funció primeDivisors :: Int -> [Int] que retorni la llista de divisors primers d’un enter estrictament positiu.
-}

myLength :: [Int] -> Int
myLength xs = length xs

myMaximum :: [Int] -> Int
myMaximum (x:xs) = foldl (max) x xs

average :: [Int] -> Float
average (x:xs) = fromIntegral(foldl (+) x xs) / fromIntegral((length xs) + 1) 

buildPalindrome :: [Int] -> [Int]
buildPalindrome xs = reverse xs ++ xs

remove2 :: [Int] -> Int -> [Int]
remove2 xs y
  | elem y xs = remove2 (part1++(drop ((length part1)+1) xs)) y
  | otherwise = xs
  where part1 = takeWhile (/= y) xs

remove :: [Int] -> [Int] -> [Int]
remove xs [] = xs
remove xs (y:ys)
  | elem y xs  = remove (remove2 xs y) ys
  | otherwise = remove xs ys


flatten :: [[Int]] -> [Int]
flatten xs = concat xs


oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens xs = ((filter odd xs),(filter even xs))

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = noDivs x (x - 1)


noDivs :: Int -> Int -> Bool
noDivs x 1 = True
noDivs x y =
  if mod x y == 0 then False
  else noDivs x (y - 1)
       


divsList :: Int -> Int -> [Int]
divsList x 1 = []
divsList x y =
  if (mod x y == 0) && isPrime y then 
    l ++ [y]
  else l
  where l = divsList x (y - 1)
        
primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors x
  | isPrime x = [x]
  | otherwise = divsList x (x - 1)
