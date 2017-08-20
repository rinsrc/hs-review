import Data.List

-- (1) multiples of 3 and 5
prob1 :: Integer
prob1 = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..999]

-- (2) even fibonacci numbers
prob2 :: Integer
prob2 = (sum . filter even . takeWhile (< 4000000)) fibs
    where
        fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- (3) largest prime factor
prob3 :: Integer
prob3 = last $ primeFactors 600851475143

-- if length of the prime factors of a number is 1, then it is a prime number
primes :: [Integer]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

-- list of prime factors for a number n
primeFactors :: Integer -> [Integer]
primeFactors n = f n primes
    where
        f n (x:xs)
            | x*x > n = [n]
            | n `mod` x == 0 = x : f (n `div` x) (x:xs)
            | otherwise = f n xs

-- (4) largest palindrome product
prob4 :: Integer
prob4 = maximum $ filter isPalindrome (f [100..999] [100..999] [])
    where
        isPalindrome n = reverse (show n) == (show n)
        f [] _ zs = zs
        f (x:xs) (ys) zs = f xs ys (zs ++ map (*x) ys)
