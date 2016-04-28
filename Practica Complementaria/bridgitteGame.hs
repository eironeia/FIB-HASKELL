import System.Random
import Debug.Trace

data Taulell = Taulell [[Char]]


instance Show Taulell where
    show (Taulell []) = ""
    show (Taulell (fila:filas)) = filaescrita++"\n"++(show (Taulell filas)) 
        where filaescrita = (concat $ map (\x -> (escriuCaracter (fila!!x) x)) [0..((length fila) -1)])

mostraGuanyador :: Taulell -> [(Int, Int)] -> Int -> String
mostraGuanyador (Taulell []) cami numFila = "\x1b[0m"
mostraGuanyador (Taulell (fila:filas)) cami numFila = filaescrita++"\n"++(mostraGuanyador (Taulell filas) cami (numFila + 1))
    where filaescrita = (concat $ map (\x -> (escriuCaracterGuanyador (fila!!x) numFila x cami)) [0..((length fila) -1)])
          
escriuCaracterGuanyador :: Char -> Int -> Int -> [(Int, Int)] -> String
escriuCaracterGuanyador car n m cami
    |elem (n, m) cami = escriu 2 [' ', car]
    |otherwise = escriuCaracter car m

escriuCaracter :: Char -> Int -> String
escriuCaracter car pos = escriu jugador [' ', car]
    where jugador = if (car == '—') then 1 - (mod pos 2)
                                    else  (mod pos 2)
escriu :: Int -> [Char] -> String
escriu jugador s
    |jugador == 2 = "\x1b[34m"++s++"\x1b[0m"
    |jugador == 1 = "\x1b[32m"++s++"\x1b[0m"
    |otherwise = "\x1b[31m"++s++"\x1b[0m"

creaTaulell :: Int -> Int -> Taulell
creaTaulell n m = Taulell taulell
    where
        taulell = map (\x -> if even x then filaparell else filasenar) [0..(2*m)]
        filaparell = map (\x -> ' ' ) [0..(2*n)]
        filasenar = map (\x -> if even x then '*' else ' ') [0..(2*n)]
        
llargada :: Taulell -> Int 
llargada (Taulell m) = (length m)

amplada :: Taulell -> Int
amplada (Taulell m) = (length (m!!0))

updateLlista :: [a] -> Int -> a -> [a]
updateLlista l j valor = esquerra++[valor]++(tail dreta)
    where 
        (esquerra, dreta) = splitAt j l
        
updateMatriu :: [[Char]] -> Int -> Int -> Char -> [[Char]]
updateMatriu m i j valor = esquerra++[(updateLlista (m!!i) j valor)]++(tail dreta)
    where 
        (esquerra, dreta) = splitAt i m

realitzaMoviment :: Taulell -> Moviment -> Taulell
realitzaMoviment (Taulell t) (Moviment (c,n,m)) = (Taulell (updateMatriu t n m c))

posicionsLliures :: Taulell -> [(Int, Int)]
posicionsLliures taulell = filter (\(x,y) -> posicioBuida taulell x y) [(x, y) | x<-[0..(n-1)], y <-[0..(m -1)]]
    where
        n = llargada taulell
        m = amplada taulell
    
posicioBuida :: Taulell -> Int -> Int -> Bool
posicioBuida (Taulell t) n m = ((t!!n)!!m) == ' '

getCaracter :: Int -> Int -> Char
getCaracter indexJugador pos
    | (mod pos 2) == indexJugador = '|'
    | otherwise = '—'
    
treuRepetits :: Eq a => [a] -> [a] -> [a]
treuRepetits [] x = x
treuRepetits (x:xs) res 
    | elem x res = treuRepetits xs res
    | otherwise = treuRepetits xs (x:res)

posicioBonaVertical :: Taulell -> (Int, Int) -> Bool
posicioBonaVertical (Taulell t) (i, j) 
    | (mod i 2) == 0 && (mod j 2) == 0 = ((t!!i)!!j) == '—'
    | (mod i 2) == 0 && (mod j 2) == 1 = True
    | (mod i 2) == 1 && (mod j 2) == 1 = ((t!!i)!!j) == '|'
    | otherwise = False
    

posicioBonaHoritzontal :: Taulell -> (Int, Int) -> Bool
posicioBonaHoritzontal (Taulell t) (i, j) 
    | (mod i 2) == 0 && (mod j 2) == 0 = ((t!!i)!!j) == '|'
    | (mod i 2) == 1 && (mod j 2) == 0 = True
    | (mod i 2) == 1 && (mod j 2) == 1 = ((t!!i)!!j) == '—'
    | otherwise = False

actualitzaCami :: Int -> [(Int, Int)] -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
actualitzaCami m camiVell (posicioNova, pare) = updateLlista camiVell  pos pare
    where pos = (m*(fst $ posicioNova) + (snd $ posicioNova))
    
fills:: Taulell -> [(Int, Int)] -> (Int, Int) -> Int -> [((Int, Int), (Int, Int))]
fills taulell cami (i, j) indexJugador = map (\(x,y) -> ((x,y), (i, j))) movimentsBons
    where 
        n = llargada taulell
        m = amplada taulell
        posicioBona = if (even indexJugador) then posicioBonaVertical else posicioBonaHoritzontal
        moviments = map (\x -> ((fst x) + i, (snd x) + j)) [(0,1), (0,-1), (1,0), (-1, 0)]
        movimentsDins = filter (\(x,y) -> (x >= 0) && (y >= 0) && (x < n) && (y < m)) moviments
        movimentsNous = filter (\(x, y) -> (cami!!(m*x + y)) == (-1, -1)) movimentsDins
        movimentsBons = filter (posicioBona taulell) movimentsNous

        
bfs :: Taulell-> [(Int, Int)] ->[(Int, Int)] -> Int ->[(Int, Int)]
bfs taulell [] cami indexJugador = cami
bfs taulell cua cami indexJugador = bfs taulell novaCua nouCami indexJugador
    where
        m = amplada taulell
        pas = concat $ map (\x -> fills taulell cami x indexJugador) cua
        novaCua = treuRepetits (map (fst) pas) []
        nouCami = foldl (actualitzaCami m) cami pas


victoriaJugador :: Taulell -> Int -> [(Int, Int)]
victoriaJugador taulell indexJugador
    | ultim == (-1,-1) =  []
    | even indexJugador = until (\(x:xs) -> (fst $ x) == 0) (\x -> [cami!!(m*(fst $(head x)) + (snd $(head x)))]++x) [ultim]
    | otherwise = until (\(x:xs) -> (snd $ x) == 0) (\x -> [cami!!(m*(fst $(head x)) + (snd $(head x)))]++x) [ultim]
    where 
        cua = if (even indexJugador) then map (\x -> (0, x)) (filter (odd) [0..(m - 1)])
                                     else map (\x -> (x, 0)) (filter (odd) [0..(m - 1)])
        camiInicial = if (even indexJugador) then (map (\x -> if even x then (-1, -1) else (0, x)) [0..(m -1)])++(replicate ((n - 1)*m) (-1, -1))
                                             else concat $ map (\x -> if even x then (replicate m (-1, -1)) else ((x, 0):(replicate (m - 1) (-1, -1)))) [0..(n-1)]
        cami = bfs taulell cua camiInicial indexJugador
        n = llargada taulell
        m = amplada taulell
        finals = if (even indexJugador) then filter (\x -> (cami!!((n-1)*m + x) /= (-1, -1))) [0..(m -1)]
                                        else filter (\x -> (cami!!(m*x + (m - 1)) /= (-1, -1))) [0..(n -1)]
        ultim = if (length (finals) /= 0) then if (even indexJugador) then (n - 1, (head finals))
                                                                      else ((head finals), m - 1)
                                          else (-1, -1)


insereixAmbOrdre :: [(Int,(Int,Int))] -> (Int, (Int, Int)) -> [(Int, (Int,Int))]
insereixAmbOrdre vector (cost, pos) = (takeWhile (\(c,(x,y)) -> c < cost) vector)++[(cost, pos)]++(dropWhile (\(c,(x,y)) -> c < cost) vector)

actualitzaCost :: Int -> [Int] -> (Int, (Int, Int)) -> [Int]
actualitzaCost m vcost (cost, (i,j)) = updateLlista vcost pos cost
    where pos = (m*i + j)

fillsDijkstra :: Taulell -> Int -> [Int] -> (Int, Int) -> Int -> [(Int, (Int, Int))] 
fillsDijkstra taulell cost vcost (i, j) indexJugador = movimentsNous
    where 
        n = llargada taulell
        m = amplada taulell
        posicioBona = if (even indexJugador) then posicioBonaVertical else posicioBonaHoritzontal
        moviments = map (\x -> ((fst x) + i, (snd x) + j)) [(0,1), (0,-1), (1,0), (-1, 0)]
        movimentsDins = filter (\(x,y) -> (x >= 0) && (y >= 0) && (x < n) && (y < m)) moviments
        movimentsBons = filter (\(x,y) -> (posicioBona taulell (x,y)) || (posicioBuida taulell x y)) movimentsDins
        movimentsAmbCost = map (\(x,y) -> ((costCasella taulell (x,y)), (x,y))) movimentsBons
        costCasella taulell (x,y)
            |posicioBuida taulell x y = 1 + cost
            |otherwise = cost
        movimentsNous = filter (\(c,(x,y)) -> (vcost!!(m*x + y) == -1 || (vcost!!(m*x + y)) > c)) movimentsAmbCost
    
-- trace ("Iteracio del dijkstra "++(show novaCua)++" "++(show nouVcost)) $
dijkstra :: Taulell -> [(Int, (Int, Int))] -> [Int] -> Int -> [Int]
dijkstra taulell [] vcost indexJugador = vcost
dijkstra taulell ((cost, (i,j)):cua) vcost indexJugador
    | (vcost!!(m*i + j) /= -1) && (cost > vcost!!(m*i + j)) = dijkstra taulell cua vcost indexJugador
    | otherwise =  dijkstra taulell novaCua nouVcost indexJugador
    where
        n = llargada taulell
        m = amplada taulell
        nousFills = fillsDijkstra taulell cost vcost (i,j) indexJugador
        novaCua = foldl (insereixAmbOrdre) cua nousFills
        nouVcost = foldl (actualitzaCost m) vcost nousFills
        
distancies :: Taulell -> Int -> [Int]
distancies taulell indexJugador = dijkstra taulell cuaInicial costosIncials indexJugador
    where
        n = llargada taulell
        m = amplada taulell
        cuaInicial = if (even indexJugador) then map (\x -> (0,(0, x))) (filter (odd) [0..(m - 1)])
                                            else map (\x -> (0, (x, 0))) (filter (odd) [0..(n - 1)])
        costosIncials = if (even indexJugador) then (map (\x -> if even x then (-1) else 0) [0..(m -1)])++(replicate ((n - 1)*m) (-1))
                                               else concat $ map (\x -> if even x then (replicate m (-1)) else (0:(replicate (m - 1) (-1)))) [0..(n-1)]

distanciaPerGuanyar :: Taulell -> Int -> Int
distanciaPerGuanyar taulell indexJugador = minimum (map (\x -> dist!!x) posFinals)
    where 
        n = llargada taulell
        m = amplada taulell
        dist = distancies taulell indexJugador
        posFinals = if (even indexJugador) then map (\x -> m*(n - 1) + x) (filter (odd) [0..(m - 1)])
                                           else map (\x -> m*x + (m - 1)) (filter (odd) [0..(n - 1)])
        
data Moviment = Moviment (Char,Int, Int)
instance Show Moviment where 
    show (Moviment ('—', i, j)) = "'—' a la posicio ("++(show i)++", "++(show j)++")"
    show (Moviment (c, i, j)) = (show c)++" a la posicio ("++(show i)++", "++(show j)++")"
type Estrategia = Taulell ->  IO(Moviment) 

huma :: Int ->  Taulell -> IO(Moviment)
huma indexJugador taulell = do
    putStrLn "Escull el numero de fila"
    n <- (getInt 0 ((llargada taulell) - 1))
    putStrLn "Escull el numero de columna"
    m <- (getInt 0 ((amplada taulell) - 1))
    if (not (posicioBuida taulell n m))
       then do
           putStrLn "La casella no esta buida, siusplau escull una altre"
           (huma indexJugador taulell)
       else do
           return (Moviment ((getCaracter indexJugador m), n, m))

aleatori :: Int -> Taulell -> IO(Moviment)
aleatori indexJugador taulell = do
    let possibles = posicionsLliures taulell
    mov <- randomRIO (0, (length possibles) -1)
    let (n, m) = possibles!!mov
    return (Moviment ((getCaracter indexJugador m), n, m))

simula :: Taulell -> Int -> Int -> (Int, Int) -> (Int, (Int, Int))
simula taulell indexJugador1 indexJugador2 (n, m) = (distanciaPerGuanyar  nouTaulell indexJugador2, (n, m))
    where 
        nouTaulell = (realitzaMoviment taulell (Moviment ((getCaracter indexJugador1 m), n, m)))
        
conservador :: Int -> Taulell -> IO(Moviment)
conservador indexJugador taulell = do
    let possibles = posicionsLliures taulell
    let cost = map (simula taulell (indexJugador) (indexJugador)) possibles
    let valorMaxim = fst $ maximum cost
    let possiblesBons = map (snd) (filter (\x -> (fst x) == valorMaxim) cost)
    let minimitzarBons = ( map (simula taulell indexJugador (1 - indexJugador)) possiblesBons)
    let valorMinim = fst $ minimum minimitzarBons
    let bons = map (snd) (filter (\x -> (fst x) == valorMinim) minimitzarBons)
    pos <- randomRIO (0, (length bons) - 1)
    let posicio = bons!!pos
    return (Moviment ((getCaracter indexJugador (snd posicio)), (fst posicio), (snd posicio)))

agressiu :: Int -> Taulell -> IO(Moviment)
agressiu indexJugador taulell = do
    let possibles = posicionsLliures taulell
    let cost = map (simula taulell (indexJugador) (1 - indexJugador)) possibles
    let valorMinim = fst $ minimum cost
    let possiblesBons = map (snd) (filter (\x -> (fst x) == valorMinim) cost)
    let maximitzarBons =  map (simula taulell indexJugador indexJugador) possiblesBons
    let valorMaxim = fst $ maximum maximitzarBons
    let bons = map (snd) (filter (\x -> (fst x) == valorMaxim) maximitzarBons)
    pos <- randomRIO (0, (length bons) - 1)
    let posicio = bons!!pos
    return (Moviment ((getCaracter indexJugador (snd posicio)), (fst posicio), (snd posicio)))
 
millor :: Int -> Taulell -> IO(Moviment)
millor indexJugador taulell = do
    let jo    = distanciaPerGuanyar taulell indexJugador
    let rival = distanciaPerGuanyar taulell (1 - indexJugador)
    if (jo >= rival)
       then do
           (agressiu indexJugador taulell)
       else do
           (conservador indexJugador taulell)
           
partidaUsuari :: Taulell -> Int -> Estrategia -> Estrategia -> IO()
partidaUsuari taulell torn estr1 estr2 = do
    let ans = victoriaJugador taulell 0
    let ans2 = victoriaJugador taulell 1
    if (length ans) > 0
       then do
           putStrLn (escriu 1 "Guanyador jugador 2")
           putStrLn (mostraGuanyador taulell ans 0)
           return ()
       else do
            if (length ans2) > 0
            then do
                putStrLn (escriu 0 "Guanyador jugador 1")
                putStrLn (mostraGuanyador taulell ans2 0)
                return ()
            else do
                putStrLn (show taulell)
                putStrLn (escriu torn ("Torn del jugador "++(show (torn + 1))))
                moviment <- (estr1 taulell)
                putStrLn (escriu torn ("El jugador "++(show (torn + 1))++" coloca "++(show moviment)))
                let taulellNou = (realitzaMoviment taulell moviment)
                (partidaUsuari taulellNou (1 - torn) estr2 estr1)

partidaSimulada :: Taulell -> Int -> Estrategia -> Estrategia -> IO()
partidaSimulada taulell torn estr1 estr2 = do
    let ans = victoriaJugador taulell 0
    let ans2 = victoriaJugador taulell 1
    if (length ans) > 0
       then do
           putStrLn (escriu 1 "Guanyador jugador 2")
           putStrLn (mostraGuanyador taulell ans 0)
           return ()
       else do
            if (length ans2) > 0
            then do
                putStrLn (escriu 0 "Guanyador jugador 1")
                putStrLn (mostraGuanyador taulell ans2 0)
                return ()
            else do
                moviment <- (estr1 taulell)
                putStrLn (escriu torn ("El jugador "++(show (torn + 1))++" coloca "++(show moviment)))
                let taulellNou = (realitzaMoviment taulell moviment)
                (partidaSimulada taulellNou (1 - torn) estr2 estr1)
                
escullInteligencia :: Int -> (Int -> Estrategia)
escullInteligencia n
    | n == 5 = huma
    | n == 1 = aleatori
    | n == 2 = conservador
    | n == 3 = agressiu
    | otherwise = millor
    
llegeixRival :: IO(Int -> Estrategia)
llegeixRival = do
  putStr " 1) CPU nivell molt baix \n 2) CPU nivell baix \n 3) CPU nivell normal-baix \n 4) CPU nivell normal \n 5) Huma \n"
  jug <- (getInt 1 5)
  let estr = escullInteligencia jug
  (return estr)
  
llegeixInteligencia :: IO(Int -> Estrategia)
llegeixInteligencia = do
  putStr " 1) CPU nivell molt baix \n 2) CPU nivell baix \n 3) CPU nivell normal-baix \n 4) CPU nivell normal \n"
  jug <- (getInt 1 4)
  let estr = escullInteligencia jug
  (return estr)
  
beginGame :: IO()
beginGame = do
  putStrLn "Hola benvingut!!"
  putStrLn "Escull la altura del taulell"
  n <- (getInt 1 40)
  putStrLn "Escull la amplada del taulell"
  m <- (getInt 1 40)
  putStrLn "Escull el tipus de partida:\n 1) Partida Simulada\n 2) Partida Usuari"
  tpartida <- (getInt 1 2)
  if (tpartida == 1)
     then do
         putStrLn "Escull el primer jugador: "
         jug1 <- llegeixInteligencia
         putStrLn "Escull el segon jugador: "
         jug2 <- llegeixInteligencia
         (partidaSimulada (creaTaulell n m) 0 (jug1 0) (jug2 1))
     else do
         putStrLn "Escull el rival: "
         riv <-llegeixRival
         putStrLn "Escull qui es el primer en jugar:\n 1) Jugador \n 2) Rival"
         ini <- (getInt 1 2)
         if (ini == 1)
            then do
                (partidaUsuari (creaTaulell n m) 0 (huma 0) (riv 1))
            else do
                (partidaUsuari (creaTaulell n m) 0 (riv 0) (huma 0))
  

-- Llegeix un numero que estigui en el interval [a, b] (primer i segon parametre)
getInt :: Int -> Int -> IO(Int)
getInt a b = do
    l <- getLine
    if ((not $ and $ map (\x -> x >= '0' && x <= '9') (filter (/= ' ') l)) || (length (filter (/= ' ') l) == 0))
       then do
           putStr $ "Siusplau introdueix un numero positiu entre "++(show $ a)++" i "++(show $ b)++"\n"
           ans <- (getInt a b)
           return (ans)
       else do
           let val = (read (takeWhile (/= ' ') (dropWhile (== ' ') l)))
           if (val < a || val > b)
              then do
                putStr $ "Siusplau introdueix un numero positiu entre "++(show $ a)++" i "++(show $ b)++"\n"
                ans2 <- (getInt a b)
                return (ans2)
              else do
                return (val)

main :: IO()
main = beginGame
