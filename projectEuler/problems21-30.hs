import Data.List
import System.IO
import qualified Data.Map as Map
import Data.Array
import Data.Char (digitToInt)
import Data.Maybe

-- (21) amicable numbers
prob21 :: Integer
prob21 = sum $ filter isAmi [1..10000]
    where
        -- checks if a number is amicable (a /= n and sum equal each other)
        isAmi n = n == (sum . divisors) a && n /= a
            where a = sum . divisors $ n

        -- divisors generates a list of numbers that divide into n
        divisors n = f n [1..(n `div` 2)]
            where
                f _ [] = []
                f n (x:xs)
                    | n `mod` x == 0 = x : f n xs
                    | otherwise = f n xs

-- (22) name scores
prob22 :: IO ()
prob22 = do
    handle <- openFile "textFiles/p22.txt" ReadMode
    contents <- hGetContents handle
    let names = map (\s -> read s :: String) $ words $ replaceComma contents
        -- replaceComma replaces commas in string with a space
        replaceComma "" = ""
        replaceComma (',':xs) = ' ' : replaceComma xs
        replaceComma (x:xs) = x : replaceComma xs

        -- create a map for fast alphabet lookup
        alphabet = Map.fromList $ zip ['A'..'Z'] [1..26]

        -- remove Maybe Monad
        removeM (Just x) = x

        -- letterCount assigns numeric value of an alphabetic character to a name
        letterCount [] = []
        letterCount (x:xs) = removeM (Map.lookup x alphabet) : letterCount xs

    print $ sum $ zipWith (*) [1..5163] $ map (sum . letterCount) $ sort names
    hClose handle

-- (23) non-abundant sums
-- according to WolframAlpha, limit of non-abundant sums is <= 20161, not 28123
prob23 :: Integer
prob23 = sum $ filter nonAbundantSums [1..20161]
    where
        -- use as filter to find non-abundant sums
        nonAbundantSums n
            | n < 12 = True
            | otherwise = abundantSums n $ takeWhile (<=n) abundantNums

        -- checks if a number can be expressed as the sum of two abundant number
        abundantSums _ [] = True
        abundantSums n (x:xs)
            | n `elem` (x + x : map (+x) xs) = False
            | otherwise = abundantSums n xs

        -- abundantNums creates a list of abundant numbers
        abundantNums = filter abundant [12..28123]
            where abundant n = if n < (sum $ divisors n) then True else False

        -- find list of divisors for a number n by using prime factors
        divisors n = init . nub $ (map product . powerSet . primeFactors) n

primes :: [Integer]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors :: Integer -> [Integer]
primeFactors n = f n primes
    where
        f n (x:xs)
            | x*x > n = [n]
            | n `mod` x == 0 = x : f (n `div` x) (x:xs)
            | otherwise = f n xs

powerSet :: [a] -> [[a]]
powerSet xs = foldr (\x acc -> acc ++ map (x:) acc) [[]] xs

-- (24) lexicographic permutations
prob24 :: String
prob24 = (sort . permutations $ "0123456789") !! 999999

-- (25) 1000 digit fibonacci number
prob25 :: Maybe Int
prob25 = elemIndex 1000 $ takeWhile (<1001) $ map (length . show) fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- (28) number spiral diagonals
-- some observations:
-- gridSize is always incremented by 2: [1,3,5,7,9..]
-- 4 diagonal numbers (diagNum) per gridSize (except 1): gridSize 3 has numbers [3,5,7,9], gridSize 5 has numbers [13,17,21,25]
-- last number in a gridSize is (gridSize^2): 3^2 = 9, 5^2 = 25, 7^2 = 49, ...
-- diagNum sequence is computed by (gridSize-1): gridSize 3 = ((3-1) = +2) [3,5,7,9], gridSize 5 = ((5-1) = +4) [13,17,21,25], ...
prob28 :: Integer
prob28 = sum $ f 1001 (1001^2) 0
    where
        -- f works backwards by finding 4 diagNums per gridSize
        -- starting from 1001 x 1001 gridSize, then 999 x 999 gridSize, ...
        f gridSize diagNum counter
            | gridSize == 1 = [1] -- if gridSize is 1 (center of grid), we found all diagonal numbers
            | counter == 4 = f (gridSize-2) diagNum 0 -- when counter is 4, reset to 0 and decrement grid size by 2
            | otherwise = diagNum : f gridSize (diagNum-(gridSize-1)) (counter+1)  -- calculate the 4 numbers for current gridSize

-- (29) distinct powers
prob29 :: Int
prob29 = (length . nub . concat) $ f [2..100]
    where
        -- f makes a list of lists 
        -- [ [2^2,3^2,,...100^2], [2^2,3^2,..100^2], ..[2^100,3^100,..100^100] ]
        f [] = []
        f (x:xs) = map (^x) [2..100] : f xs

-- (30) digit fifth powers
prob30 :: Int
prob30 = sum $ filter fifthPowerSum [2..upperBound]
    where
        -- upperBound determined by comparing biggest fifthPowerSum to smallest n-digit number
        -- excluse 1 digit numbers
        -- 2 digit = 9^5 * 2 > 1, range : [1..9^5*2]
        -- 3 digit = 9^5 * 3 > 10, range : [1..9^5*3]
        -- 6 digit = 9^5 * 6 > 100000, range : [1..9^5*6]
        -- 7 digit = 9^5 * 7 < 1000000
        -- at 7 digit numbers, the biggest fifthPowerSum (9^5 * 7 = 413343) is less than 1 million (smallest 7-digit number)
        -- in other words, fifthPowerSum of 7 digit numbers makes only 6 digit numbers (violates rules of problem 30)
        -- therefore, upperBound is 9^5 * 6 = 354294
        upperBound = 354294
        fifthPowerSum n = if (sum . map ((^5) . digitToInt)) (show n) == n then True else False
