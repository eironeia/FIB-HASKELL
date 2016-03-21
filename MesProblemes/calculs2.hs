

diffSqrs :: Integer -> Integer
diffSqrs 0 = 0
diffSqrs 1 = 1
diffSqrs n = (y*y) - (foldl1 (+) (map(\x -> x*x) [1..n])) 
    where y = foldl1 (+) [1..n]

 pythagoreanTriplets :: Int -> [(Int, Int, Int)]