data BTree = Empty | Node Int BTree BTree deriving Show

calcProduct :: BTree -> Int -> Int
calcProduct Empty _ = 1  -- ако дървото е празно, връщаме 1 (не влияе на произведението)
calcProduct (Node val left right) k
    | sumChildren > k = val * leftProduct * rightProduct  -- ако сумата на наследниците е по-голяма от k, умножаваме стойността на възела
    | otherwise = leftProduct * rightProduct  -- иначе просто умножаваме стойностите на наследниците
    where
    sumChildren = sum (map nodeValue [left, right])  -- изчисляваме сумата на стойностите на наследниците
    leftProduct = calcProduct left k  -- рекурсивно извикване за левия наследник
    rightProduct = calcProduct right k  -- рекурсивно извикване за десния наследник

-- Хелпер функция за извличане на стойността на възел, ако не е празен
nodeValue :: BTree -> Int
nodeValue Empty = 0
nodeValue (Node v _ _) = v

-- Примерно дърво
bt :: BTree
bt = Node 5 (Node 1 (Node 5 (Node 1 Empty Empty) (Node 2 Empty Empty))
                    (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)))
             (Node 2 (Node 1 (Node 1 Empty Empty) (Node 7 Empty Empty))
                      (Node 9 (Node 5 Empty Empty) (Node 2 Empty Empty)))

-- Main функция с тестове
main :: IO ()
main = do
    print $ calcProduct bt 2  -- Очакваме 900
    print $ calcProduct bt 6  -- Очакваме 36
    print $ calcProduct bt 7  -- Очакваме 2
    print $ calcProduct bt 8  -- Очакваме 2
