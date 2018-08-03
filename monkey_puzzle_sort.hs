data Tree a = Null | Node a (Tree a) (Tree a) deriving (Read, Show)

addNode :: Ord a => a -> Tree a -> Tree a
addNode x Null = Node x Null Null
addNode x (Node y leftTree rightTree)
   | x == y = Node y leftTree rightTree
   | x < y  = Node y (addNode x leftTree) rightTree
   | otherwise = Node y leftTree (addNode x rightTree)


makeTree :: Ord a => [a] -> Tree a
makeTree []     = Null
makeTree (x:xs) = addNode x (makeTree xs)

inOrder :: Tree a -> [a]
inOrder Null                        = []
inOrder (Node x leftTree rightTree) = inOrder leftTree ++ (x:inOrder rightTree)

mpSort :: Ord a => [a] -> [a]
mpSort xs = inOrder (makeTree xs)