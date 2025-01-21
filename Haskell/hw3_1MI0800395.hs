main :: IO ()
main = do
    print $ perimeter ["7-F7-",
                        ".FJ|7", 
                        "SJLL7", 
                        "|F--J", 
                        "LJ.LJ"]
    print $ perimeter ["-.|F7", 
                        ".S-7|", 
                        "L|7||", 
                        "-L-J|", 
                        "L|-JF"]
    print $ perimeter ["F--7", 
                        "|F-7", 
                        ".S.|", 
                        "|L-J"]
    print $ numEnclosed ["...|.....|...",
                          ".S---7..F--7.",
                          ".|...|--|..|.",
                          ".L---J..L--J.",
                          "...|.....|..."]

    print $ numEnclosed [".F--S--7.",
                          ".|F---7|.",
                          ".||...||.",
                          ".|L-7FJ|.",
                          ".L--JL-J."]

    print $ numEnclosed [".S-------7.",
                          ".|F-----7|.",
                          ".||.....||.",
                          ".|L-7.F-J|.",
                          ".|..|.|..|.",
                          ".L--J.L--J.",
                          "..........."]

    print $ numEnclosed [".S--------7.",
                          ".|.F----7.|.",
                          ".|.|....|.|.",
                          ".|.|....|.|.",
                          ".|.L-7F-J.|.",
                          ".|...||...|.",
                          ".L---JL---J."]

    print $ numEnclosed ["..F-S-7.",
                          ".FJF-7L7",
                          "FJFJ.L7|",
                          "L-J...LJ"]
type Position = (Int, Int)

data Cell = Vertical | Horizontal | TopRight | TopLeft | BottomLeft | BottomRight | Empty | Start
  deriving (Eq, Show)

parseCell :: Char -> Cell
parseCell '|' = Vertical
parseCell '-' = Horizontal
parseCell 'L' = TopRight
parseCell 'J' = TopLeft
parseCell '7' = BottomLeft
parseCell 'F' = BottomRight
parseCell '.' = Empty
parseCell 'S' = Start
parseCell _   = error "Invalid cell"

neighbors :: Cell -> Position -> [Position]
neighbors Vertical (x, y) = [(x - 1, y), (x + 1, y)]
neighbors Horizontal (x, y) = [(x, y - 1), (x, y + 1)]
neighbors TopRight (x, y) = [(x - 1, y), (x, y + 1)]
neighbors TopLeft (x, y) = [(x - 1, y), (x, y - 1)]
neighbors BottomLeft (x, y) = [(x + 1, y), (x, y - 1)]
neighbors BottomRight (x, y) = [(x + 1, y), (x, y + 1)]
neighbors Start (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
neighbors Empty _ = []

neighbors2 :: Position -> [Position]
neighbors2 (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isConnected :: Cell -> Cell -> Position -> Position -> Bool
isConnected from to pos1 pos2 = pos2 `elem` neighbors from pos1 && pos1 `elem` neighbors to pos2

getCell :: [String] -> Position -> Cell
getCell grid (x, y)
  | x >= 0 && y >= 0 && x < length grid && y < length (head grid) = parseCell (grid !! x !! y)
  | otherwise = Empty

bfsCycle :: [String] -> [Position] -> [Position] -> [Position]
bfsCycle grid visited [] = visited
bfsCycle grid visited (current:queue)
  | current `elem` visited = bfsCycle grid visited queue
  | otherwise = bfsCycle grid (current : visited) (queue ++ next)
  where
    currentCell = getCell grid current
    next = [neighbor | neighbor <- neighbors currentCell current, neighbor `notElem` visited, isConnected currentCell (getCell grid neighbor) current neighbor]

bfsCycle2 :: [String] -> [Position] -> [Position] -> [Position]
bfsCycle2 grid visited [] = visited
bfsCycle2 grid visited (current:queue)
  | current `elem` visited = bfsCycle grid visited queue
  | otherwise = bfsCycle grid (current : visited) (queue ++ next)
  where
    currentCell = getCell grid current
    next = [neighbor | neighbor <- neighbors2 current,
             neighbor `notElem` visited,
             let neighborCell = getCell grid neighbor,
             neighborCell /= Empty]

findStart :: [String] -> Position
findStart [] = error "No start position"
findStart grid = head [(x, y) | x <- [0 .. length grid - 1], y <- [0 .. length (head grid) - 1], grid !! x !! y == 'S']

bfsEnclosed :: [String] -> [Position] -> [Position] -> [Position] -> Bool -> (Bool, [Position])
bfsEnclosed grid visited [] component isEnclosed = (isEnclosed, component)
bfsEnclosed grid visited (current:queue) component isEnclosed
  | current `elem` visited = bfsEnclosed grid visited queue component isEnclosed
  | isBoundary = bfsEnclosed grid (current : visited) queue component False
  | otherwise = bfsEnclosed grid (current : visited) (queue ++ next) (current : component) isEnclosed
  where
    isBoundary = fst current < 0 || snd current < 0 || fst current >= length grid || snd current >= length (head grid)
    next = [neighbor | neighbor <- neighbors2 current,
             neighbor `notElem` visited,
             getCell grid neighbor == Empty]

numEnclosed :: [String] -> Int
numEnclosed grid = length distinctComponents
  where
    start = findStart grid
    cycleCells = bfsCycle grid [] [start]
    emptyCells = [(x, y) | x <- [0 .. length grid - 1], y <- [0 .. length (head grid) - 1], 
                   getCell grid (x, y) == Empty, 
                   (x, y) `notElem` cycleCells]
    
    distinctComponents = findDistinctComponents grid emptyCells [] []

findDistinctComponents :: [String] -> [Position] -> [Position] -> [[Position]] -> [[Position]]
findDistinctComponents _ [] _ components = components
findDistinctComponents grid (cell:queue) visited components
  | cell `elem` visited = findDistinctComponents grid queue visited components
  | otherwise = let (isEnclosed, component) = bfsEnclosed grid visited [cell] [] True
                in if isEnclosed
                   then findDistinctComponents grid queue (visited ++ component) (component : components)
                   else findDistinctComponents grid queue (visited ++ component) components

perimeter :: [String] -> Int
perimeter grid = length $ bfsCycle grid [] [findStart grid]