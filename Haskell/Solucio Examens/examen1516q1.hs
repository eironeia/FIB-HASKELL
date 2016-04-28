

-- 1.1
quadrats::[Integer]
quadrats = [x*x | x <- [1..]]

-- 1.2

auxSumQ:: Integer -> Integer
auxSumQ n = head $ dropWhile (< n) (map (\x -> sum $ take x quadrats) [1..])

auxSumQ2:: Integer -> Integer -> Integer
auxSumQ2 n y = head $ dropWhile (> n) (map (\x -> y - (sum $ take x quadrats)) [1..])

sumQuadrats::Integer -> Bool
sumQuadrats n
    | n == sumq = True
    | n == sumq2 = True
    | otherwise = False
    where sumq = (auxSumQ n)
          sumq2 = (auxSumQ2 n sumq)

-- 2

conwayAux:: [Int] -> [Int]
conwayAux xs = xs++(conwayAux (xs++[(xs!!((last xs)))+(xs!!((length xs) - (last xs)))]))

conway::[Int]
conway = conwayAux [1,1]

-- 3



-- 4

--Node "a" [Node "b" [Node "b" [Node "d" [], Node "e" []],Node "c" [],Node "c" []], Node "a" []]
--Node "a" [Node "b" [Node "d" [],Node "e" [],Node "c" [],Node "c" []]]

data GTree a = Node a [GTree a] deriving (Show)

-- 4.1

flat:: (GTree a) -> (GTree a)
flat (Node x []) = (Node x [])
flat (Node arrel ((Node arrel2 fills2):fills))

flat (Node arrel (fills2++fills))

{-
Node a => Fills: [b,a]
Node b => Fills: [b,c,c]
Node b => Fills: [d,e]


-}



