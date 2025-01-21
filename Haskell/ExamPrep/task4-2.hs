data BTree = Empty | Node Int BTree BTree deriving Show

-- Главната функция
generateNum :: BTree -> Int -> Int
generateNum bt k
  | k <= 0    = 0  -- Ако нивото е невалидно
  | otherwise = digitsToNumber $ collectDigits bt k 0
  where
    -- Помощна функция за събиране на подходящи цифри
    collectDigits :: BTree -> Int -> Int -> [Int]
    collectDigits Empty _ _ = []
    collectDigits (Node val left right) k currentLevel
      | currentLevel == k - 1 = case left of
          Node lv _ _ -> lv : collectDigits left k (currentLevel + 1)
          _           -> collectDigits left k (currentLevel + 1)
      | otherwise = collectDigits left k (currentLevel + 1) ++ collectDigits right k (currentLevel + 1)

    -- Функция за превръщане на списък от цифри в число
    digitsToNumber :: [Int] -> Int
    digitsToNumber [] = 0
    digitsToNumber xs = read (concatMap show xs)

-- Примерни дървета
t1 :: BTree
t1 = Node 6 (Node 3 (Node 2 Empty Empty)
                  (Node 5 (Node 4 Empty Empty)
                          Empty))
            (Node 8 (Node 7 Empty Empty)
                    (Node 9 Empty Empty))

t2 :: BTree
t2 = Node 4 (Node 1 Empty
                  (Node 3 Empty Empty))
            (Node 5 Empty
                  (Node 7 (Node 6 Empty Empty)
                          Empty))

-- Тестове
main :: IO ()
main = do
  print $ generateNum t1 1 -- → 3
  print $ generateNum t1 2 -- → 27
  print $ generateNum t1 3 -- → 4
  print $ generateNum t2 1 -- → 1
  print $ generateNum t2 2 -- → 0
  print $ generateNum t2 3 -- → 6
