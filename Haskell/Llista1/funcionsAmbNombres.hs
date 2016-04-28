{-

Feu una funció absValue :: Int -> Int que, donat un enter, retorni el seu valor absolut.

Feu una funció power :: Int -> Int -> Int que, donats un enter x i un natural p, retorni x elevat a p, és a dir, xp.

Feu una funció isPrime :: Int -> Bool que, donat un natural, indiqui si aquest és primer o no.

Feu una funció slowFib :: Int -> Int que retorni l’n-èsim element de la sèrie de Fibonacci tot utilitzant l’algorisme recursiu que la defineix 
(f(0)=0, f(1)=1, f(n)=f(n−1)+f(n−2) per n≥ 2).

Feu una funció quickFib :: Int -> Int que retorni l’n-èsim element de la sèrie de Fibonacci tot utilitzant un algorisme més eficient.

-}

absValue :: Int -> Int
absValue x
  | x < 0 = (-x)
  | otherwise = x


power :: Int -> Int -> Int
power x 0 = 1
power x p = x*(power x (p-1))


isPrime :: Int -> Bool
isPrime x =  (length $ filter (\y -> (mod x y) == 0) [2..(div x 2)]) == 0

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib x = slowFib(x-1) + slowFib(x-2)
