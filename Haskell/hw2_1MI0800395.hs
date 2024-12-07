main :: IO ()
main = do
    print $ isValidSudoku
        [[1, 2, 3, 4, 5, 6, 7, 8],
         [5, 6, 7, 8, 1, 2, 3, 4],
         [2, 3, 4, 1, 6, 5, 8, 7],
         [6, 5, 8, 7, 2, 3, 4, 1],
         [3, 4, 1, 2, 7, 8, 5, 6],
         [7, 8, 5, 6, 3, 4, 1, 2],
         [4, 1, 2, 3, 8, 7, 6, 5],
         [8, 7, 6, 5, 4, 1, 2, 3]]
    print $ isValidSudoku
        [[1, 6, 3, 4, 5, 6, 7, 8],
         [5, 2, 7, 8, 1, 2, 3, 4],
         [2, 3, 4, 1, 6, 5, 8, 7],
         [6, 5, 8, 7, 2, 3, 4, 1],
         [3, 4, 1, 2, 7, 8, 5, 6],
         [7, 8, 5, 6, 3, 4, 1, 2],
         [4, 1, 2, 3, 8, 7, 6, 5],
         [8, 7, 6, 5, 4, 1, 2, 3]]
    print $ isValidSudoku
        [[1, 2, 3, 4, 5, 6, 7, 8],
         [5, 6, 7, 8, 1, 2, 3, 4],
         [2, 3, 4, 1, 6, 5, 8, 7],
         [6, 5, 8, 7, 2, 3, 4, 1],
         [3, 4, 1, 2, 2, 8, 5, 6],
         [7, 8, 5, 6, 3, 4, 1, 2],
         [4, 1, 2, 3, 8, 7, 6, 5],
         [8, 7, 6, 5, 4, 1, 2, 3]]
         
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldr (\x seen -> if x `elem` seen then seen else x:seen) []

contains1to8Once :: [Int] -> Bool
contains1to8Once xs = all (`elem` xs) [1..8] && length (removeDuplicates xs) == 8

checkRows :: [[Int]] -> Bool
checkRows = all contains1to8Once

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose matrix = map head matrix : transpose (map tail matrix)

checkCols :: [[Int]] -> Bool
checkCols matrix = checkRows (transpose matrix)

getSubgrids :: [[Int]] -> [[Int]]
getSubgrids matrix = concatMap (map concat . chunkList 4) (chunkList 2 matrix)

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)

checkSubgridsValid :: [[Int]] -> Bool
checkSubgridsValid matrix = all contains1to8Once (getSubgrids matrix)

isValidSudoku :: [[Int]] -> Bool
isValidSudoku matrix = checkRows matrix && checkCols matrix && checkSubgridsValid matrix         