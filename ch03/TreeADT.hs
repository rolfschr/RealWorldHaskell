-- file: ch03/TreeADT.hs
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
    deriving (Show)
