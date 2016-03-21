

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show) 

-- donat un arbre, retorni la seva talla, és a dir, el nombre de nodes que conté.
size :: Tree a -> Int
size Empty = 0
size (Node n fe fd) = 1 + (size fe) + (size fd)

-- donat un arbre, retorni la seva alçada, assumint que els arbres buits tenen alçada zero.
height :: Tree a -> Int 
height Empty = 0;
height (Node n fe fd) = 1 + max (height fe) (height fd)

-- donat dos arbres, indiqui si són el mateix.
equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty (Node n2 fe2 fd2) = False
equal (Node n1 fe1 fd1) Empty = False
equal (Node n1 fe1 fd1) (Node n2 fe2 fd2) = (n1 == n2) && (equal fe1 fe2) && (equal fd1 fd2)

-- si es pot obtenir l’un de l’altre tot girant algun dels seus fills.
isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty (Node n2 fe2 fd2) = False
isomorphic (Node n1 fe1 fd1) Empty = False
isomorphic (Node n1 fe1 fd1) (Node n2 fe2 fd2) = (n1 == n2) && cond1 && cond2 && cond3 && cond4
    where cond1 = (fe1 == fe2) || (fe1 == fd2)
          cond2 = (fd1 == fe2) || (fd1 == fd2)
          cond3 = isomorphic fe1 fe2
          cond4 = isomorphic fd1 fd2
