import Data.List
main :: IO()
main = do
    print $ nodes [(1, 2), (1, 3), (2, 3), (2, 4)] == [1, 2, 3, 4]

    print $ neighbors 2 [(1, 2), (1, 3), (2, 3), (2, 4)] == [3, 4]
    print $ neighbors 4 [(1, 2), (1, 3), (2, 3), (2, 4)] == []

    print $ adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)] == [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]

type Graph a = [Edge a]
type Edge a = (a, a)
type Node a = a

nodes :: (Eq a) => Graph a -> [a]
nodes [] = []
nodes es = nub $ concat [[from, to] | (from, to) <- es]

neighbors :: (Eq a) => Node a -> Graph a -> [Node a]
neighbors node es = [to | (from, to) <- es, from == node]

adjacencyList :: (Eq a) => Graph a -> [(Node a, [Node a])]
adjacencyList es = map (\ node -> (node, (neighbors node es))) $ nodes es


