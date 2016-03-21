
{-
Feu una funció eql :: [Int] -> [Int] -> Bool que indiqui si dues llistes d’enters són iguals.
Feu una funció prod :: [Int] -> Int que calculi el producte dels elements d’una llista d’enters.
Feu una funció prodOfEvens :: [Int] -> Int que multiplica tots el nombres parells d’una llista d’enters.
Feu una funció powersOf2 :: [Int] que generi la llista de totes les potències de 2.
Feu una funció scalarProduct :: [Float] -> [Float] -> Float que calculi el producte escalar de dues llistes de reals de la mateixa mida.
-}

eql :: [Int] -> [Int] -> Bool
eql xs ys
  | (length xs) /= (length ys) = False
  | otherwise = and $ zipWith (==) xs ys



prod :: [Int] -> Int
prod xs = (product xs)

prodOfEvens :: [Int] -> Int
prodOfEvens xs = product (filter even xs)


powersOf2 :: [Int]
powersOf2 =  iterate (2*) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs ys = sum $ zipWith (*) xs ys