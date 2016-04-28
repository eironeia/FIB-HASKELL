import System.Random
import Debug.Trace


data Taulell = Taulell [[[Char]]] 

-- Creem un taulell per jugar de n*m dimensions
creaTaulell:: Int -> Int -> Taulell
creaTaulell n m = Taulell taulell
    where taulell = map (\x -> if even x then columnes else spaces) [0..(2*n)]
          columnes = map (\x -> if even x then ['*'] else [' ']) [0..(2*m)]
          spaces  = map (\x -> [' ']) [0..(2*m)]

instance Show Taulell where
    show (Taulell []) = ""
    show (Taulell (fila:filas)) = (concat fila)++"\n"++(show (Taulell filas)) 


-- Retornem totes les coordenades on es troben els '*'
coordPunts:: Taulell -> Int -> [[Int]]
coordPunts (Taulell []) _ = []
coordPunts (Taulell (fila:filas)) n = coordFilaActual ++ (coordPunts (Taulell filas) (n+1))
  where coordFilaActual = filter (/= []) $ map (\x -> if ((fila!!x) == "*") then [n,x] else []) [0..((length fila)-1)]

-- Mirem si la coordenada que s'indica [n,m] existeix realment un punt
existeixPunt:: Taulell -> Int -> Int -> Bool
existeixPunt taulell n m = elem [n,m] (coordPunts taulell  0)

-- Posicio correcta donada una coord [x,y] mira si està dintre del tablero
between::[Int] -> Int -> Int -> Bool
between [x,y] n m = (x >= 0) && (x <= 2*n) && (y >= 0) && (y <= 2*m)


-- Comprova si la casella [x,y] està lliure en el Taulell
posicioLliure::Taulell -> [Int] -> Bool
posicioLliure (Taulell filas) [x,y] = ((filas!!x)!!y) == " "

-- Direccio de la barra 0:Top 1: Bottom 2:Right 3: Left
-- Mirem si podrem colocar la barra on es desitja tenint en compte si hi ha una barra o estem fora del taulell
esPotColocarBarra::[Int] -> Taulell -> Int -> Bool
esPotColocarBarra [x,y] (Taulell filas) 0 = (between [(x-1),(y)] (length filas) (length $ (filas!!1))) && (posicioLliure (Taulell filas) [(x-1),(y)])
esPotColocarBarra [x,y] (Taulell filas) 1 = (between [(x+1),(y)] (length filas) (length $ (filas!!1))) && (posicioLliure (Taulell filas) [(x+1),(y)])
esPotColocarBarra [x,y] (Taulell filas) 2 = (between [(x),(y-1)] (length filas) (length $ (filas!!1))) && (posicioLliure (Taulell filas) [(x),(y-1)])
esPotColocarBarra [x,y] (Taulell filas) 3 = (between [(x),(y+1)] (length filas) (length $ (filas!!1))) && (posicioLliure (Taulell filas) [(x),(y+1)])

-- Afegir una barra en el taulell en el punt en coord [x,y] 
--                        coord  "- or |"   0     
afegirBarra::Taulell -> [Int] -> String -> Int ->  Taulell
afegirBarra (Taulell []) [x,y] s _ = Taulell []
afegirBarra (Taulell (fila:filas)) [x,y] s n
  | x == n = Taulell ([concatMap (\k -> if k == y then [s] else [fila!!k]) [0..((length fila)-1)]] ++ f2)
  | otherwise = Taulell ([fila]++f2)
  where (Taulell f2) = afegirBarra (Taulell filas) [x,y] s (n+1)


-- mirar quants quadrats ha fet jugador1
numQuadratsA::Taulell -> Int
numQuadratsA (Taulell []) = 0
numQuadratsA (Taulell (fila:filas)) = (sum $ map(\x -> if (fila!!x) == "A" then 1 else 0) [0..((length fila)-1)]) + (numQuadratsA (Taulell filas))


-- mirar quants quadrats ha fet jugador 2
numQuadratsB::Taulell -> Int
numQuadratsB (Taulell []) = 0
numQuadratsB (Taulell (fila:filas)) = (sum $ map(\x -> if (fila!!x) == "B" then 1 else 0) [0..((length fila)-1)]) + (numQuadratsB (Taulell filas))




------------- ENTRADA -------------



{-
-- Llegeix un numero que estigui en el interval [a, b] (primer i segon parametre)
getInt :: Int -> Int -> IO(Int)
getInt a b = do
    l <- getLine
    if ((not $ and $ map (\x -> x >= '0' && x <= '9') (filter (/= ' ') l)) || (length (filter (/= ' ') l) == 0))
       then do
           putStr $ "introdueix un numero positiu entre "++(show $ a)++" i "++(show $ b)++"\n"
           ans <- (getInt a b)
           return (ans)
       else do
           let val = (read (takeWhile (/= ' ') (dropWhile (== ' ') l)))
           if (val < a || val > b)
              then do
                putStr $ "introdueix un numero positiu entre "++(show $ a)++" i "++(show $ b)++"\n"
                ans2 <- (getInt a b)
                return (ans2)
              else do
                return (val)

beginGame :: IO()
beginGame = do
  putStrLn "Benvingut"
  putStrLn "Escull la altura del taulell"
  n <- (getInt 1 20)
  putStrLn "Escull la amplada del taulell"
  m <- (getInt 1 20)
  putStrLn "Escull el tipus de partida:\n 1) Partida Simulada\n 2) Partida Usuari"


main :: IO()
main = beginGame

-}

