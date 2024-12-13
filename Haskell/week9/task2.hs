main :: IO()
main = do
    print $ constructMaxBTree [3, 2, 1, 6, 0, 5] == (Node 6 (Node 3 Nil (Node 2 Nil (Node 1 Nil Nil))) (Node 5 (Node 0 Nil Nil) Nil))

data BTree a = Nil | Node a (BTree a) (BTree a) deriving (Show, Eq, Functor)

constructMaxBTree :: [Int] -> BTree Int
constructMaxBTree [] = Nil
constructMaxBTree xs = Node maxEl (constructMaxBTree $ takeWhile (/= maxEl) xs) (constructMaxBTree $ tail $ dropWhile (/= maxEl) xs)
 where
    maxEl = maximum xs
