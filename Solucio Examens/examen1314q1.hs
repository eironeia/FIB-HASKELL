

-- 1
data Arbre a = Node a (Arbre a) (Arbre a) | Abuit deriving (Show)

ttake::(Arbre a) -> Int -> (Arbre a)
ttake (Abuit) _ = Abuit
ttake (Node a fe fd) 1 = (Node a Abuit Abuit)
ttake (Node a fe fd) n = (Node a (ttake fe (n-1)) (ttake fd (n-1)))


inftreeAux:: Int -> (Arbre Int)
inftreeAux n = (Node n (inftreeAux (n+1)) (inftreeAux (n+1)))

inftree::(Arbre Int)
inftree = inftreeAux 1

--Node 1 (Node 2 (Node 3 (Node 4 Abuit Abuit) (Node 4 Abuit Abuit)) (Node 3 (Node 4 Abuit Abuit) (Node 4 Abuit Abuit))) (Node 2 (Node 3 (Node 4 Abuit Abuit) (Node 4 Abuit Abuit)) (Node 3 (Node 4 Abuit Abuit) (Node 4 Abuit Abuit)))
--Node 1 (Node 2 (Node 3 Abuit Abuit) (Node 3 Abuit Abuit)) (Node 2 (Node 3 Abuit Abuit) (Node 3 Abuit Abuit))
--Node 1 (Node 2 (Node 3 Abuit Abuit) (Node 3 Abuit Abuit)) (Node 2 (Node 3 Abuit Abuit) (Node 3 Abuit Abuit))
