 2.3
Probant coses:

-- Entrada

Seq[Assign "x" (Const 1), Assign "y" (Const 1), Print (Plus (Var "x") (Const 3))]

-- Sortida

x := 1 ;
y := 1 ;
PRINT x + 3 ;



-- Entrada

Cond (AND  (Gt (Const 4) (Const 0)) (Eq (Const 4) (Const 0))) (Assign "x" (Const 1)) (Cond (AND  (Gt (Const 4) (Const 0)) (Eq (Const 4) (Const 0))) (Assign "x" (Const 1)) (Assign "x" (Const 1)))

-- Sortida

IF 4 > 0 AND 4 = 0 THEN
   x := 1 ;
END

-- Entrada

Cond (OR  (Gt (Const 4) (Var "X")) (NOT (Eq (Const 0) (Var "Y")))) (Assign "x" (Const 1)) (Assign "x" (Const 1))

-- Sortida

IF 4 > X OR NOT 0 = Y THEN
   x := 1 ;
END

-- Entrada

Cond (AND  (Gt (Const 4) (Const 0)) (Eq (Const 4) (Const 0))) (Seq[(Assign "x" (Const 1)), (Input "Y")])

-- Sortida
IF 4 > 0 AND 4 = 0 THEN
   x := 1 ;
   INPUT Y;
END


-- Entrada

Loop (Gt (Const 4) (Var "X")) (Seq[Assign "x" (Const 1), Assign "y" (Const 1), Print (Plus (Var "x") (Const 3))]) 

-- Sortida
WHILE 4 > X
DO
	x := 1 ;
	y := 1 ;
PRINT x + 3;





-----

let b = (Node ("X",0) EmptyT EmptyT)
let b2 = update b "Y" 3
interpretCommand b2 [1,2] (Loop (Gt (Const 5) (Var "X") ) (Seq[(Assign "X" (Plus (Var "X") (Const 1))), (Print (Var "X"))]))  

(Left [1,2,3,4,5],Node ("X",5) EmptyT (Node ("Y",3) EmptyT EmptyT),[1,2])




interpretProgram [1,2,3] (Seq[(Assign "X" (Const 1)), (Loop (Gt (Const 5) (Var "X")) (Assign "X" (Plus (Var "X") (Const 1))))])

interpretProgram [1,2,3] (Seq[(Assign "X" (Const 1)), (Loop (Gt (Const 5) (Var "X")) (Seq[(Assign "X" (Plus (Var "X") (Const 1)))])), (Assign "X" (Plus (Var "X") (Const 1))), Print (Var "X")])



interpretProgram [1,2,3] (Seq[(Assign "X" (Const 1)), (Seq[(Assign "X" (Plus (Var "X") (Const 1)))]), (Assign "X" (Plus (Var "X") (Const 1))), Print (Var "X")])


(Loop (Gt (Const 5) (Var "X")) (Seq[(Assign "X" (Plus (Var "X") (Const 1)))]))


, (Print (Var "X"))
(Left [3],Node ("X",2) EmptyT (Node ("Y",3) EmptyT EmptyT),[1,2])

:l practica2016.hs
let b = (Node ("X",2) EmptyT EmptyT)
let b2 = update b "Y" 3	
interpretCommand b2 [1,2] (Cond (Gt (Var "X") (Const 0)) (Seq[(Assign "x" (Const 1))]) (Print (Var "X")))




interpretProgram [1,2] (Seq [Assign "X" (Const 0), (Seq[(Loop (Gt (Const 5) (Var "X")) (Seq[Assign "X" (Plus (Var "X") (Const 1)), Print (Var "X")]))])])

interpretProgram [1,2] (Seq [Assign "X" (Const 0), (Seq[(Seq[Assign "X" (Plus (Var "X") (Const 1)), Print (Var "X")])])])




-- PETA
interpretCommand b [1] (Seq[Assign "X" (Const 1), (Loop (Gt (Const 5) (Var "X")) (Seq[(Print (Var "X")),(Assign "X" (Plus (Var "X") (Const 1)))]))])


(Seq [(Input "X"), (Cond )])



(Seq [(Input "X"), (Input "Y"), (Cond (OR (Gt (Var "X") (Const 0)) (OR (Eq (Var "X") (Const 0)) (NOT (Gt (Const 0) (Var "Y"))))) (Seq [(Assign "Z" (Const 1)), (Loop (Gt (Var "X") (Var "Y")) (Seq [(Assign "X" (Minus (Var "X") (Const 1))), (Assign "Z" (Times (Var "Z") (Var "Z")))])) ])  (Assign "Z" (Const 0))), (Print (Var "Z"))])










(Seq [(Input "X"),( Assign "X"(Const 1)), (Cond (AND  (Gt (Const 4) (Const 0)) (Eq (Const 4) (Const 0))) (Assign "x" (Const 1)) (Assign "x" (Const 1)))])


	

