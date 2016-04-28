
{-
---------------------------------------------------------- 
| Practica Haskell 2015-2016 Q2 *Llenguatge imperatiu*.	 |
| Alex Cuello				 				 			                       |
| Llenguatges de Programacio, FIB						             |
----------------------------------------------------------
-}

-- 2 Generació del l’arbre de sintaxi abstracta (AST)

-- 2.1 Definició de dates

type Ident = String

data Command c = Assign Ident (NumEx c) 
                | Input Ident 
                | Print (NumEx c) 
                | Seq [Command c] 
                | Cond (BoolEx c) (Command c) (Command c)
                | Loop (BoolEx c) (Command c) 

data NumEx a = Const a 
                | Var Ident 
                | Plus (NumEx a) (NumEx a) 
                | Minus (NumEx a) (NumEx a) 
                | Div (NumEx a) (NumEx a) 
                | Times (NumEx a) (NumEx a)

data BoolEx a = Gt (NumEx a) (NumEx a) 
                | Eq (NumEx a) (NumEx a) 
                | AND (BoolEx a) (BoolEx a) 
                | OR (BoolEx a) (BoolEx a) 
                | NOT (BoolEx a)

-- 2.2 readCommand

{- Està a mitges però no hi ha gaire cosa

readCommand :: (Read a)=> String -> Command a
readCommand "" = Seq []

genBoolEx::(Read a) => [String] -> BoolEx a
genBoolEx ls
  | (rightS == []) = genComp ls
  | (head rightS) == "AND" = AND (genBoolEx leftS) (genBoolEx $ tail rightS)
  | (head rightS) == "OR" = OR (genBoolEx leftS) (genBoolEx $ tail rightS)
  | (head rightS) == "NOT" = NOT $ genBoolEx $ tail rightS
  where (leftS,rightS) = span (\c->c/="AND" && c/="OR" && c/="NOT") ls

genComp::(Read a) => [String] -> BoolEx a
genComp ls
  | comp == "=" = Eq (genNumEx rightS) (genNumEx $ tail leftS)
  | comp == ">" = Gt (genNumEx rightS) (genNumEx $ tail leftS)
  where (leftS,rightS) = span (\c->c/=">" && c/="=") ls
        comp = head leftS;

genNumEx::(Read a) => [String] -> NumEx a
genNumEx (x:xs)
  | elem "+" (x:xs) = Plus (genNumEx $ leftSum) (genNumEx $ tail $ rightSum)
  | elem "-" (x:xs) = Minus (genNumEx $ leftMinus) (genNumEx $ tail $ rightMinus)
  | elem "*" (x:xs) = Times (genNumEx $ leftTimes) (genNumEx $ tail $ rightTimes)
  | elem "/" (x:xs) = Div (genNumEx $ leftDiv) (genNumEx $ tail $ rightDiv)
  | elem (head x) ['A'..'Z'] = Var x
  | otherwise = Const (read x)
  where (leftSum, rightSum) = span (/="+") (x:xs)
        (leftMinus, rightMinus) = span (/="-") (x:xs)
        (leftTimes, rightTimes) = span (/="*") (x:xs)
        (leftDiv, rightDiv) = span (/="/") (x:xs)
-}

-- 2.3 Show data

instance Show n => Show (NumEx n) where
  show (Const c) = show c
  show (Var v) = v
  show (Plus  x y) = concat [show x," + ",show y]
  show (Minus x y) = concat [show x," - ",show y]
  show (Div x y) = concat [show x," / ",show y]
  show (Times x y) = concat [show x," * ",show y]

instance Show b => Show (BoolEx b) where
  show (Gt x y) =  concat [show x," > ", show y]
  show (Eq x y) =  concat [show x," = ", show y]
  show (AND x y) =  concat [show x," AND ", show y]
  show (OR x y) =  concat [show x," OR ", show y]
  show (NOT x) =  concat ["NOT ",show x]

myShow:: Show a => String -> Command a -> String
myShow espai (Assign s eN) = concat [espai,s," := ", show eN,";\n"]
myShow espai (Input i) = concat [espai,"INPUT ", i,";\n"]
myShow espai (Print eN) = concat [espai,"PRINT ", show eN,";\n"]
myShow espai (Seq []) = ""
myShow espai (Seq lc) = concatMap (\x -> myShow espai x) lc
myShow espai (Cond eB c1 c2 ) = concat [espai,"IF ", show eB, " THEN\n", myShow (espai++"  ") c1, espai++"ELSE\n", myShow (espai++"  ") c2, espai++"END\n"]
myShow espai (Loop eB c) = concat [espai,"WHILE ", show eB, "\n"++espai++"DO\n", myShow (espai ++ "  ") c,espai++"END\n" ]


instance Show c => Show (Command c) where
 show c = myShow " " c

-- 3 Interpret

-- 3.1 Class SymTable

class SymTable m where
  update :: m a -> String -> a -> m a
  value :: m a -> String -> a
  exists :: m a -> String -> Bool
  start :: m a 

-- 3.2 Memory of SymTable Class

data Memory m = Mem [(String, m)] deriving (Show) 

instance SymTable Memory where -- Asumim que sempre hi haurà la variable en qüestió? I si es que no, quin valor ha de tornar en els casos que no ho sigui?
  update (Mem []) var value = Mem ((var,value):[])
  update (Mem ((v,x):l)) var value
    | var == v = Mem ((var,value):l)
    | otherwise = Mem ((v,x):ys)
    where
      Mem ys = (update (Mem l) var value)

  value (Mem ((v,x):ls)) var
    | v == var = x
    | otherwise = value (Mem ls) var

  exists (Mem []) _ = False
  exists (Mem ((v,x):ls)) var
    | v == var = True
    | otherwise = exists (Mem ls) var

  start = Mem []

-- 3.3 Memory of SymTamble Class with Binary Tree

data MemoryT a = Node (String,a) (MemoryT a) (MemoryT a) | EmptyT deriving (Show)

instance SymTable MemoryT where
  update (EmptyT) var value = Node (var,value) EmptyT EmptyT
  update (Node (var1,val1) fe fd) var value
    | var1 == var = Node (var,value) fe fd
    | var1 > var = Node (var1,val1) (update fe var value) fd
    | otherwise = Node (var1,val1) fe (update fd var value)

  value (Node (var1,val1) fe fd) var
    | var1 == var = val1
    | var1 > var = value fe var
    | otherwise = value fd var

  exists EmptyT _ = False
  exists (Node (var1,val1) fe fd) var
    | var1 == var = True
    | var1 > var =  exists fe var
    | otherwise = exists fd var

  start = EmptyT


-- 3.4 Interpret Command function

-- Aux funcionts in Numeric Expressions

resultNegPos::(Num a, Ord a) => a -> a -> a -> a
resultNegPos x y r
  | (x >= 0 && y >= 0) || (x < 0 && y < 0) = r
  | otherwise = (-r)

negToPos::(Num a, Ord a) => a -> a
negToPos x
  | x >= 0 = x
  | otherwise = (-x)

mydiv :: (Num a, Ord a) => a -> a -> a
mydiv 0 y = 0
mydiv x y
  | x >= y = 1+(mydiv (x-y) y)
  | otherwise = 0

-- Numerical Expressions Evaluated in interpretCommand

evalNumEx:: (SymTable m, Num a, Ord a) => NumEx a -> m a -> (Either a String)
evalNumEx (Const c) mem = Left c
evalNumEx (Var v) mem 
  | (exists mem v) = Left (value mem v)
  | otherwise = Right ("Undefined variable: " ++ v)
evalNumEx (Plus ne1 ne2) mem = checkNum (+) (evalNumEx ne1 mem) (evalNumEx ne2 mem)
evalNumEx (Minus ne1 ne2) mem = checkNum (-) (evalNumEx ne1 mem) (evalNumEx ne2 mem)
evalNumEx (Div ne1 ne2) mem = checkDiv (evalNumEx ne1 mem) (evalNumEx ne2 mem)
evalNumEx (Times ne1 ne2) mem = checkNum (*) (evalNumEx ne1 mem) (evalNumEx ne2 mem)


-- Errors in Numerical Expressions

checkNum:: (Num a, Ord a) => (a -> a -> a) -> (Either a String) -> (Either a String) -> (Either a String)
checkNum f (Left x) (Left y) = Left (f x y)
checkNum _ (Right s1) _ = Right s1
checkNum _ _ (Right s2) = Right s2

checkDiv:: (Num a, Ord a) => (Either a String) -> (Either a String) -> (Either a String)
checkDiv (Left a) (Left b)
  | b == 0 = Right ("Division by zero")
  | otherwise =  Left (resultNegPos a b (mydiv (negToPos a) (negToPos b)))
checkDiv (Right a) _ = Right a
checkDiv _ (Right b) = Right b


-- Boolean Expressions Evaluated in interpretCommand

evalBoolEx:: (SymTable m, Num a, Ord a) => BoolEx a -> m a -> (Either Bool String)
evalBoolEx (Gt ne1 ne2) mem = checkEqGt (>) (evalNumEx ne1 mem) (evalNumEx ne2 mem)
evalBoolEx (Eq ne1 ne2) mem = checkEqGt (==) (evalNumEx ne1 mem) (evalNumEx ne2 mem)
evalBoolEx (AND be1 be2) mem = checkAndOr (&&) (evalBoolEx be1 mem) (evalBoolEx be2 mem)
evalBoolEx (OR be1 be2) mem = checkAndOr (||) (evalBoolEx be1 mem) (evalBoolEx be2 mem)
evalBoolEx (NOT be) mem = checkNot (evalBoolEx be mem)
 
-- Errors in Bool Expressions

checkNot:: (Either Bool String) -> (Either Bool String)
checkNot (Left x) = Left (not x)
checkNot (Right s) = Right s

checkAndOr:: (Bool -> Bool -> Bool) -> (Either Bool String) -> (Either Bool String) -> (Either Bool String)
checkAndOr f (Left b1) (Left b2) = Left (f b1 b2)
checkAndOr _ (Right s1) _ = Right s1
checkAndOr _ _ (Right s2) = Right s2
 
checkEqGt:: (Num a, Ord a) => (a -> a -> Bool) -> (Either a String) -> (Either a String) -> (Either Bool String)
checkEqGt f (Left x) (Left y) = Left (f x y)
checkEqGt _ (Right s1) _ = Right s1
checkEqGt _ _ (Right s2) = Right s2

-- Interpret Command function

interpretCommand :: (SymTable m, Num a, Ord a) => m a -> [a] -> Command a -> ((Either [a] String),m a, [a])
interpretCommand mem xs (Assign var en)
  | (Left x) <- exprN = (Left [], (update mem var x), xs)
  | (Right y) <- exprN = (Right y, mem, xs)
  where exprN = (evalNumEx en mem)

interpretCommand mem [] (Input _) = (Right "Empty input list", mem, [])
interpretCommand mem (x:xs) (Input v) = (Left [],(update mem v x), (x:xs))

interpretCommand mem xs (Print en)
  | (Left x) <- evalEN = (Left [x], mem, xs)
  | (Right s) <- evalEN = (Right s, mem, xs)
  where evalEN = (evalNumEx en mem)

interpretCommand mem xs (Cond eb c1 c2)
  | (Right y) <- (evalBoolEx eb mem) = (Right y, mem, xs)
  | x = interpretCommand mem xs c1
  | otherwise = interpretCommand mem xs c2
    where (Left x) = (evalBoolEx eb mem)

interpretCommand mem inputL (Seq cL) = mySeq mem inputL cL []
interpretCommand mem inputL (Loop be c) = myLoop mem inputL be c []

-- Aux functions to interpretCommand

mySeq:: (SymTable m, Num a, Ord a) => m a -> [a] -> [Command a] -> [a] -> ((Either [a] String),m a, [a])
mySeq mem inputL [] printL = (Left printL, mem, inputL)
mySeq mem inputL (l:lc) printL
  | (Left [], mem2, inputL2) <- seqComand = mySeq mem2 inputL2 lc printL
  | (Left [x], mem2, inputL2) <- seqComand = mySeq mem2 inputL2 lc (printL++[x])
  | (Right s, mem2, inputL2) <- seqComand = (Right s, mem2, inputL2)
  where seqComand = interpretCommand mem inputL l

myLoop:: (SymTable m, Num a, Ord a) => m a -> [a] -> BoolEx a -> Command a -> [a] -> ((Either [a] String),m a, [a])
myLoop mem inputL be c printL
  | (Right y) <- (evalBoolEx be mem) = (Right y, mem, inputL)
  | x = myLoop2 mem inputL be c printL
  | otherwise = (Left printL , mem, inputL)
  where (Left x) = (evalBoolEx be mem)

myLoop2:: (SymTable m, Num a, Ord a) => m a -> [a] -> BoolEx a -> Command a -> [a] -> ((Either [a] String),m a, [a])
myLoop2 mem inputL be c printL 
  | (Right y,mem2,inputL2) <- evalCom = (Right y, mem2, inputL2) 
  | (Left [], mem2, inputL2) <- evalCom = myLoop mem2 inputL2 be c printL
  | (Left [xs], mem2, inputL2) <- evalCom = myLoop mem2 inputL2 be c (printL++[xs])
  where evalCom = interpretCommand mem inputL c

-- 3.5 interpret Program

fstT:: (SymTable m, Num a, Ord a) => ((Either [a] String),m a, [a]) -> (Either [a] String)
fstT (Left[], mem, inputL) = Left []
fstT (Left [xs], mem, inputL) = Left [xs]
fstT (Right y, mem, inputL) = Right y

interpretProgram:: (Num a,Ord a) => [a] -> Command a -> (Either [a] String)
interpretProgram inputL c = fstT $ interpretCommand (start::MemoryT c) inputL c

-- 4 Copy Detection

-- 4.1 Expand Function

expand :: Command a -> Command a
expand (Seq cs) = Seq (map (expand) cs)
expand (Loop eb c) = (Loop eb (expand c))
expand (Cond eb c1 c2) = myExpand eb (expand c1) (expand c2)
expand c = c

myExpand:: BoolEx a -> Command a -> Command a -> Command a
myExpand (AND eb1 eb2) c1 c2 = (Cond eb1 (myExpand eb2 c1 c2) c2)
myExpand (OR eb1 eb2) c1 c2 = (Cond eb1 c1 (myExpand eb2 c1 c2))
myExpand eb c1 c2 = (Cond eb c1 c2)


-- 4.2 Simplify Function

mySimplify :: Command a -> Command a
mySimplify (Seq cs) = Seq (map (mySimplify) cs)
mySimplify (Cond eb1 c1 c2) = (Cond eb1 (mySimplify c1) (mySimplify c2))
mySimplify (Loop eb c) = (Loop eb (mySimplify c))
mySimplify c = Seq []

mySimplify2:: Command a -> Command a
mySimplify2 (Seq []) = Seq []
mySimplify2 (Seq cs) = Seq (map (mySimplify2) (filter (/= (Seq [])) cs))
mySimplify2 (Cond eb c1 c2) = (Cond eb (mySimplify2 c1) (mySimplify2 c2))
mySimplify2 (Loop eb c) = (Loop eb (mySimplify2 c))

simplify:: Command a -> Command a
simplify c = mySimplify2 (mySimplify c)


-- 4.3 Isomorf Function

isomorf:: Command c -> Command c -> Bool
isomorf (Seq []) (Seq []) = True
isomorf (Seq xs) (Seq ys)
  | length xs == length ys = and $ zipWith (isomorf) xs ys
  | otherwise = False
isomorf (Cond eb1 c1 c2) (Cond eb2 c3 c4) = ((isomorf c1 c3) && (isomorf c2 c4)) || ((isomorf c1 c4) && (isomorf c2 c3))
isomorf (Loop eb1 c1) (Loop eb2 c2) = isomorf c1 c2
isomorf _ _ = False

  
instance Eq (Command a) where
    c1 == c2 = isomorf (simplify $ expand c1) (simplify $ expand c2)

