{-
Feu una funció flatten :: [[Int]] -> [Int] que aplana una llista de llistes d’enters en una llista d’enters.
Feu una funció myLength :: String -> Int que retorna la llargada d’una cadena de caràcters.
Feu una funció myReverse :: [Int] -> [Int] que inverteix els elements d’una llista d’enters.
Feu una funció countIn :: [[Int]] -> Int -> [Int] que, donada una llista de llistes d’elements ℓ i un element x ens torna la llista que indica quants cops apareix x en cada llista de ℓ.
Feu una funció firstWord :: String -> String que, donat un string amb blancs i caràcacters alfabètics), en retorna la primera paraula.
-}



flatten :: [[Int]] -> [Int]
flatten a = foldl (++) [] a

myLength :: String -> Int
myLength a = foldl (\y x -> y + 1) 0 a

myReverse :: [Int] -> [Int] 
myReverse a = foldr (\x y -> y++[x]) [] a

countIn :: [[Int]] -> Int -> [Int]
countIn x a = map (\l -> length (filter (== a) l)) x

firstWord :: String -> String 
firstWord x = takeWhile (/= ' ') (dropWhile (== ' ') x)