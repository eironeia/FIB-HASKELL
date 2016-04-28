

-- 1.1

inflists::[[Integer]]
inflists = map (\x -> [x..]) [1..]

-- 1.2

sumAux:: Integer -> [Integer] -> [Integer]
sumAux x ys = map (\x -> sum $ take x ys ) [1..((length ys)-1)]

takeLESum :: Integer -> [Integer] -> [Integer]
takeLESum x l = take y l
    where y = length $ takeWhile (<= x) (tail (scanl (+) 0 l))
          
consecutSum :: Integer -> [Integer]
consecutSum a = takeLESum a (head (dropWhile (\x -> sum (takeLESum a x) /= a) inflists))


-- Problema 3

-- 3.1

data Expressio a = Fulla a | Binari (a -> a -> a) (Expressio a) (Expressio a) | Unari (a -> a) (Expressio a)

aval::Expressio a -> a
aval (Fulla a) = a
aval (Binari f a b) = f (aval a) (aval b)
aval (Unari f a) = f (aval a)

instance (Ord a) => Eq (Expressio a) where
    a == b = (aval a) == (aval b)

data NExpressio a = NFulla a | NUnari (a -> a) (NExpressio a) | Nari (a -> a -> a) [NExpressio a]

naval::NExpressio a -> a
naval (NFulla a) = a
naval (NUnari f a) = f (naval a)
naval (Nari f xs) = foldl1 (f) (map (\x -> naval x) xs)
