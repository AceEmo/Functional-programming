main :: IO()
main = do
    print $ Circle 5
    print $ Triangle 5 6 7
    print $ Circle 5 == Circle 10
    print $ Circle 5 == Circle 5
    print $ f "Rectangle 10 5" == Rectangle 10 5
    print $ Circle 5 < Circle 10
    print $ Circle 5 < Circle 1
    print $ f "Triangle 5 6 7"
    print $ f "Triangle 5.55 6.154 7.484"

data Shape  a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
    deriving (Show, Eq, Read, Ord)

f :: String -> Shape Float
f = read