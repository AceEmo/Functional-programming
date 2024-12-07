main :: IO()
main = do
    print $ myImages (\x -> x * x) (2+) [Point 2 2, Point 1 2, Point 3 7] == [Point 2 2, Point 3 7]
    print $ myImagesFold (\x -> x * x) (2+) [Point 2 2, Point 1 2, Point 3 7] == [Point 2 2, Point 3 7]

data Point2D a = Point a a
    deriving (Show, Eq)
myImages :: (Eq a) => (a -> a) -> (a -> a) -> [Point2D a] -> [Point2D a]
myImages f g = filter (\ (Point x y) -> f x == g y)

myImagesFold :: (Eq a) => (a -> a) -> (a -> a) -> [Point2D a] -> [Point2D a]
myImagesFold f g = foldr (\ p@(Point x y) res -> if f x == g y then p : res else res) []
