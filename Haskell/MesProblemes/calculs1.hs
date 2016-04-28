{-
Feu una funció sumMultiples35 :: Integer -> Integer que, donat un natural n, retorni la suma de tots els múltiples de 3 o de 5 per sota de n.
Feu una funció fibonacci :: Int -> Integer que, donat un natural n, retorni l’n-èsim elements de la sèrie de Fibonacci.
Feu una funció sumEvenFibonaccis :: Integer -> Integer que, donat un natural n, retorni la suma de tots els elements parells inferiors a n de la sèrie de Fibonacci.
Feu una funció largestPrimeFactor :: Int -> Int que, donat un natural n≥1, retorna el factor primer més gran de n.
Feu una funció isPalindromic :: Integer -> Bool que, donat un natural n, retorni si n és palindròmic, és a dir, si n es llegeix igual del dret que del revés.

-}

mult3:: Integer -> [Integer]
mult3 n = [ 3*x | x <- [1..n], (3*x) < n]

mult5:: Integer -> [Integer]
mult5 n = [ 5*x | x <- [1..n], (5*x) < n]

sumMultiples35 :: Integer -> Integer
sumMultiples35 x = foldl (+) 0 ((mult5 x)++(mult3 x))

fibonacci :: Int -> Integer
fibonacci  n = fromIntegral(fibs!!n)

fibs:: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

takeLessN n = takeWhile (< n) fibs

sumEvenFibonaccis :: Integer -> Integer
sumEvenFibonaccis n = foldl1 (+) (filter (even) (takeLessN n))

largestPrimeFactor :: Int -> Int
largestPrimeFactor n = last $ factors n

isPrime:: Int -> Bool
isPrime x = null $ filter (\y ->  x `mod` y == 0) $ takeWhile (\y ->  y*y <= x) [2..]

factors:: Int -> [Int]
factors n = [x | x <- [2..n], (isPrime x) && (mod n x == 0)]

-- isPalindromic 9062609
isPalindromic :: Integer -> Bool
isPalindromic n = True







