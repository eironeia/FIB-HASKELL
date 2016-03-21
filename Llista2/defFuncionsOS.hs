{-










-}

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ z []     = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = [a] ++ (myIterate f (f a))

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil c f a
    | c a = a
    | otherwise = myUntil c f (f a)

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = [f x]++(myMap f xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter c [] = []
myFilter c (x:xs)
    | c x = [x] ++ myFilter c xs
    | otherwise = myFilter c xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll c [] = True
myAll c (x:xs)
    | c x = True && (myAll c xs)
    | otherwise = False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs)
    | f x = True
    | otherwise = myAny f xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = [(x,y)]++(myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = [f x y] ++ (myZipWith f xs ys)



