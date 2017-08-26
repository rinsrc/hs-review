import Data.List
import System.IO
import Data.Char (digitToInt)
import Data.Array

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

-- (5) smallest multiple
prob5 :: Integer
-- foldr1 takes 2 elements from list (start from end) and applies function to it
prob5 = foldr1 lcm [1..20]

-- (6) sum square difference
prob6 :: Integer
prob6 = sum [1..100]^2 - (sum . map (^2)) [1..100]

-- (7) 10001st prime number
prob7 :: Integer
prob7 = primes !! 10000 -- primes from prob3 (10000, b/c index starts at 0)

-- (8) largest product in a series
prob8 :: IO ()
prob8 = do
    -- number is stored in separate text file
    handle <- openFile "p8.txt" ReadMode
    contents <- hGetContents handle

    -- use array for fast lookup of each digit individually
    let arr = listArray (0,999) $ map digitToInt $ filter (/='\n') contents
        -- findMax gets the maximum product of 13 adjacent digits in array
        findMax = f 0 12 1
            where
                f _ 1000 max = max
                f start end max =
                    if adjacentProd > max
                        then f (start+1) (end+1) adjacentProd
                        else f (start+1) (end+1) max
                    where
                        -- multiplies 13 adjacent digits created by slice function
                        adjacentProd = product $ map snd (slice start)

                        -- slice creates a subarray of 13 digits
                        slice start = take 13 (drop start $ assocs arr)
    print findMax
    hClose handle

-- (9) special pythagorean triplet
prob9 :: Integer
prob9 = (round . product . head) $ filter kSum $ filter isInt $ f 0 [1..500] [2..501] []
    where
        -- kSum checks if sum of pythagorean triplet equals 1000
        kSum xs = if sum xs == 1000 then True else False

        -- isInt checks if c is a an integer or whole number
        isInt [a,b,c] = if c == fromIntegral (floor c) then True else False

        -- f generates list of lists of potential pythagorean triplets
        f _ [] _ triplets = triplets
        f 500 (a:as) bs triplets = f 0 as bs triplets
        f counter (a:as) bs triplets
            | (bs !! counter) < a = f (counter+1) (a:as) bs triplets -- prevents duplicate triplets
            | otherwise = f (counter+1) (a:as) bs ([a,(bs !! counter),c] : triplets)
                where c = sqrt $ a^2 + (bs !! counter)^2

-- (10) summation of primes
prob10 :: Integer
prob10 = sum $ takeWhile (<2000000) primes
