import Data.List (maximumBy)
import Data.Ord (comparing)

-- Дефиниция на бинарно дърво
data Tree a = Empty | Node a (Tree a) (Tree a)

-- Функция за изчисляване на сумите на възлите на всяко ниво
levelSums :: Num a => Tree a -> [(Int, a)]
levelSums tree = levelSumsHelper tree 1
  where
    levelSumsHelper Empty _ = []
    levelSumsHelper (Node x left right) level = 
        (level, x) : (levelSumsHelper left (level + 1)) ++ (levelSumsHelper right (level + 1))

-- Функция за намиране на нивото с най-голяма сума
maxLevelSum :: (Ord a, Num a) => Tree a -> Int
maxLevelSum tree = fst $ maximumBy (comparing snd) (sumLevels tree)
  where
    sumLevels t = map (\lvl -> (lvl, sum [val | (l, val) <- levelSums t, l == lvl])) [1..maxLevel t]
    maxLevel Empty = 0
    maxLevel (Node _ left right) = 1 + max (maxLevel left) (maxLevel right)

-- Примерно бинарно дърво
exampleTree :: Tree Int
exampleTree = Node 1
                  (Node 2 
                    (Node 4 Empty Empty)
                    (Node 5 Empty Empty)
                  )
                  (Node 3
                    (Node 6 Empty Empty)
                    (Node 7 Empty Empty)
                  )

-- Main функция с I/O
main :: IO ()
main = do
    print $ maxLevelSum exampleTree
