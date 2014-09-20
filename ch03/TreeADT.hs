-- file: ch03/TreeADT.hs
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
    deriving (Show)

simpleTree = Node "parent" (Just (Node "left child" Nothing Nothing))
                           (Just (Node "right child" Nothing (Just (Node "rr" Nothing Nothing))))


height :: Tree a -> Int
height (Node a Nothing Nothing) = 1
height (Node a (Just b) Nothing) = 1 + height b
height (Node a Nothing (Just b)) = 1 + height b
height (Node a (Just b) (Just c)) = 1 + max (height b) (height c)

height' :: Maybe (Tree a) -> Int
height' Nothing = 0
height' (Just (Node _ l r)) = 1 + max (height' l) (height' r)
