{-
Feu una funció myMap :: (a -> b) -> [a] -> [b] que emuli el map usant llistes per comprensió.
Feu una funció myFilter :: (a -> Bool) -> [a] -> [a] que emuli el filter usant llistes per comprensió.
Feu una funció myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] que que emuli el zipWith usant llistes per comprensió i zip.
Feu una funció thingify :: [Int] -> [Int] -> [(Int, Int)] que, donades dues llistes d’enters, genera la llista que aparella els elements si l’element de la segona llista divideix al de la primera.
Feu una funció factors :: Int -> [Int] que, donat un natural no nul, genera la llista ordenada amb els seus factors (no necessàriament primers).
-}

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [f x | x <- xs]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter c xs = [x | x <- xs, c x]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = [ f x y | (x,y) <- (zip xs ys)]

thingify :: [Int] -> [Int] -> [(Int, Int)] 
thingify xs ys = [ (x,y) | x <- xs, y <- ys, (mod x y) == 0]

factors :: Int -> [Int]
factors y = [x | x <- [1..y], mod y x == 0]