data Queue a = Queue [a] [a] deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue xs ys) = Queue xs ([a]++ys)

pop :: Queue a -> Queue a
pop (Queue [] []) = Queue [] []
pop (Queue [] ys) = Queue (drop 1 (reverse ys)) []
pop (Queue (x:xs) ys) = Queue xs ys

top :: Queue a -> a
top (Queue [] ys) = last ys
top (Queue (x:xs) _) = x

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue _ _) = False

instance Eq a => Eq (Queue a) where 
    (Queue x1 y1) == (Queue x2 y2) = (x1++(reverse y1)) == (x2++(reverse y2))

