{-
Feu una funció countIf :: (Int -> Bool) -> [Int] -> Int que, donat un predicat sobre els enters i una llista d’enters, retorna el nombre d’elements de la llista que satisfan el predicat.
Nota: Aquesta funció d’ordre superior existeix en llenguatges de tractament de fulls de càlcul com ara EXCEL.

Feu una funció pam :: [Int] -> [Int -> Int] -> [[Int]] que, donada una llista d’enters i una llista de funcions d’enters a enters, retorna la llista de llistes resultant d’aplicar cada una de les funcions de la segona llista als elements de la primera llista.
Feu una funció pam2 :: [Int] -> [Int -> Int] -> [[Int]] que, donada una llista d’enters i una llista de funcions d’enters a enters, retorna la llista de llistes on cada llista és el resultat d’aplicar successivament les funcions de la segona llista a cada element de la primera llista.
Nota: Qualsevol semblança amb La parte contratante de la primera parte será considerada como la parte contratante de la primera parte és pura casualitat.

Feu una funció filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int que fa el plegat dels elements que satisfan la propietat donada.
Feu una funció insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int] que donada una relació entre enters, una llista i un element, ens retorna la llista amb l’element inserit segons la relació.
Utilitzant la funció insert, feu una funció insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int] que ordeni la llista per inserció segons la relació donada.

-}

countIf :: (Int -> Bool) -> [Int] -> Int
countIf f [] = 0
countIf f xs = length ([x | x <- xs, f x])

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam xs fs = map (\y -> map (\x -> y x) xs) fs

pam2 :: [Int] -> [Int -> Int] -> [[Int]]    
pam2 xs fs = map (\y -> map (\x -> x y) fs) xs

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl c f x ys = foldl (f) x ([y | y <- ys, c y])

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert c [] y = [y]
insert c (x:xs) y
    | c x y = [x] ++ (insert c xs y)
    | otherwise = [y] ++ [x] ++ xs

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort c [] = []
insertionSort c xs = insertionSaux c xs [] 

insertionSaux:: (Int -> Int -> Bool) ->[Int] -> [Int] -> [Int]
insertionSaux c [] ys = ys
insertionSaux c (x:xs) ys = insertionSaux c xs (insert c ys x)






