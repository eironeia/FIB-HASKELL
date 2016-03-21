
{-

Feu una funció size :: Tree a -> Int que, donat un arbre, retorni la seva talla, és a dir, el nombre de nodes que conté.
Feu una funció height :: Tree a -> Int que, donat un arbre, retorni la seva alçada, assumint que els arbres buits tenen alçada zero.
Feu una funció equal :: Eq a => Tree a -> Tree a -> Bool que, donat dos arbres, indiqui si són el mateix.
Feu una funció isomorphic :: Eq a => Tree a -> Tree a -> Bool que, donat un arbres, indiqui si són el isomorfs, és a dir, si es pot obtenir l’un de l’altre tot girant algun dels seus fills.
Feu una funció preOrder :: Tree a -> [a] que, donat un arbre, retorni el seu recorregut en pre-ordre.
Feu una funció postOrder :: Tree a -> [a] que, donat un arbre, retorni el seu recorregut en post-ordre.
Feu una funció inOrder :: Tree a -> [a] que, donat un arbre, retorni el seu recorregut en in-ordre.
Feu una funció breadthFirst :: Tree a -> [a] que, donat un arbre, retorni el seu recorregut per nivells.
Feu una funció build :: Eq a => [a] -> [a] -> Tree a que, donat el recorregut en pre-ordre d’un arbre i 
el recorregut en in-ordre del mateix arbre, retorni l’arbre original. Assumiu que l’arbre no té elements repetits.
Feu una funció overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a que, donats dos arbres, 
retorni la seva superposició utilitzant una funció. Superposar dos arbres amb una funció consisteix 
en posar els dos arbres l’un damunt de l’altre i combinar els nodes doble resultants amb la funció donada o deixant els nodes simples tal qual.

-}

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node n fe fd) = 1+(size fe)+(size fd)

height :: Tree a -> Int
height Empty = 0
height (Node n fe fd) = 1 + max (height fe) (height fd)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal _ Empty = False
equal Empty _ = False
equal (Node n1 fe1 fd1) (Node n2 fe2 fd2)
    | n1 == n2 = (equal fe1 fe2) && (equal fd1 fd2)
    | otherwise = False

isomorphic::Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic _ Empty = False
isomorphic Empty _ = False
isomorphic (Node x fe1 fd1) (Node y fe2 fd2) = (x == y) && (((isomorphic fe1 fe2) && (isomorphic fd1 fd2)) || ((isomorphic fe1 fd2) && (isomorphic fd1 fe2)) )

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node n fe fd) = [n]++(preOrder fe)++(preOrder fd)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node n fe fd) = (postOrder fe)++(postOrder fd)++[n]


inOrder :: Tree a -> [a] 
inOrder Empty = []
inOrder (Node n fe fd) = (inOrder fe) ++[n]++(inOrder fd)

auxBread:: Tree a -> Tree a -> [a]
auxBread (Node n1 fe1 fd1) (Node n2 fe2 fd2) = [n1]++[n2]

nodeTree:: Tree a -> [a]
nodeTree Empty = []
nodeTree (Node n fe fd) = [n]

breadthFirst::Tree a -> [a]
breadthFirst Empty = []
breadthFirst a = (nodeTree a) ++ breadthFirstAux a


breadthFirstAux :: Tree a -> [a]
breadthFirstAux Empty = []
breadthFirstAux (Node n fe fd) = (nodeTree fe)++(nodeTree fd)++(breadthFirstAux fe)++(breadthFirstAux fd)










