main :: IO ()
main = do
    print $ multiplyDigitsRecursion 1234
    print $ sumDigitsRecursion 1234
    print $ reverseNumberRecursion 1234
    print $ reverseNum 1234
    print $ checkOdd 11371791
    print $ checkOddRecursive 1137913
    print $ numToXs 1234567
    print $ reversedODD 12
    print $ reversibleNumbers 20
    print $ reversibleNumbers 31
    print $ reversibleNumbers 10

sumDigits :: Int -> Int
sumDigits n = sum $ map (\x -> read [x] :: Int) $ show n

sumDigitsRecursion :: Int -> Int
sumDigitsRecursion n
    | n < 10 = n
    | otherwise = n `mod` 10 + sumDigitsRecursion (n `div` 10)

multiplyDigitsRecursion :: Int -> Int
multiplyDigitsRecursion n
    | n < 10 = n
    | otherwise = n `mod` 10 * multiplyDigitsRecursion (n `div` 10)

reverseNumberRecursion :: Int -> Int
reverseNumberRecursion n = helper n 0
    where
        helper 0 reversed = reversed
        helper num reversed = helper (num `div` 10) (reversed * 10 + num `mod` 10)

reverseNum :: Int -> Int
reverseNum n = read (reverse (show n)) :: Int

checkOddRecursive :: Int -> Bool
checkOddRecursive 0 = True
checkOddRecursive n
    | odd (n `mod` 10) = checkOddRecursive (n `div` 10)
    | otherwise = False

checkOdd :: Int -> Bool
checkOdd n = all (`elem` "13579") (show n)


numToXs :: Integer -> [Int]
numToXs 0 = [0]
numToXs n
    | n < 0     = numToXs (-n)
    | otherwise = reverse (helper n)
  where
    helper 0 = []
    helper x = fromInteger (x `mod` 10) : helper (x `div` 10)

reversedODD :: Int -> Bool
reversedODD n = checkOdd (n + reverseNum n)

reversibleNumbers :: Int -> [Int]
reversibleNumbers n = filter (\x -> reversedODD x && x `mod` 10 /= 0) [1..n]
