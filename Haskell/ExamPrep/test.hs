primes :: [Int]
primes = sieve [2..]  -- Infinite list of numbers, we filter out non-primes using the sieve

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]  -- Only keep numbers not divisible by p

isEven :: Int -> Bool
isEven = even

main :: IO()
main = do
    print $ take 10 primes
