{-
factorials :: [Integer]
fibs :: [Integer]
primes :: [Integer]
hammings :: [Integer]
lookNsay :: [Integer]
tartaglia :: [[Integer]]
Generar la seqüència de les files del triangle de Tartaglia (també anomenat triangle de Pascal): [[1],[1,1],[1,2,1],[1,3,3,1],…].
-}

-- Generar la seqüència dels uns [1,1,1,1,1,1,1,1,…].

ones :: [Integer]
ones = cycle [1]

seqUns:: [Int]
seqUns = map (\x -> 1) [1..]

-- Generar la seqüència dels naturals [0,1,2,3,4,5,6,7…].

nats :: [Integer]
nats = iterate (+1) (0)

-- Generar la seqüència dels enters [0,1,−1,2,−2,3,−3,4…].

ints :: [Integer]
ints = [0]++(auxEnt (drop 1 nats) (map (\x -> (-x) ) (drop 1 nats)))

auxEnt:: [Integer] -> [Integer] -> [Integer]
auxEnt (x:xs) (y:ys) = [x]++[y]++(auxEnt xs ys)


-- Generar la seqüència dels nombres triangulars: 0,1,3,6,10,15,21,28,…].

triangulars :: [Integer]
triangulars = map (\x -> div (x*(x+1)) 2) nats

-- Generar la seqüència dels nombres factorials: [1,1,2,6,24,120,720,5040,…].

auxFact::Integer -> Integer
auxFact 1 = 1
auxFact x = x * (auxFact (x-1))

factorials :: [Integer]
factorials = [1] ++ (map (\x -> auxFact x) (drop 1 nats))

-- Generar la seqüència dels nombres de Fibonacci: [0,1,1,2,3,5,8,13,…].

fibs::[Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Generar la seqüència dels nombres primers: [2,3,5,7,11,13,17,19,…].

divideix::[Integer]->Integer->Bool
divideix [] p = False
divideix (x:xs) p =
    if (x*x > p) then False
                 else if ((mod p x) == 0) then True
                                          else divideix xs p
isPrime::Integer->Bool
isPrime p = not (divideix (tail (tail nats)) p)
primes::[Integer]
primes = filter (isPrime) (tail (tail nats))

-- Generar la seqüència ordenada dels nombres de Hamming: [1,2,3,4,5,6,8,9,…]. Els nombres de Hamming són aquells que només tenen 2, 3 i 5 com a divisors primers.

uneix::[Integer]->[Integer]->[Integer]
uneix x [] = x
uneix [] y = y
uneix (x:xs) (y:ys) =
    if (x < y) then x:(uneix xs (y:ys))
               else if (y < x) then y:(uneix (x:xs) ys)
                               else uneix (x:xs) ys
hammings::[Integer]
hammings = 1 : uneix (map (2*) hammings) (uneix (map (3*) hammings) (map (5*) hammings))

say::String->String
say [] = []
say (x:xs) = (show (length y))++[x]++(say (dropWhile (== x) (x:xs)))
    where y = (takeWhile (== x) (x:xs))         
nextSay::Integer -> Integer
nextSay f = read (say (show f))::Integer
lookNsay::[Integer]
lookNsay = (iterate nextSay 1)

nexTar::[Integer] -> [Integer]
nexTar f = [1]++(zipWith (+) f (tail f))++(1:[])
tartaglia::[[Integer]]
tartaglia = (iterate nexTar [1])








