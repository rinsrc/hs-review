import Data.List
import System.IO
import qualified Data.Map as Map
import Data.Array
import Data.Char
import Data.Maybe

-- (21) amicable numbers
prob21 :: Int
prob21 = sum $ filter isAmi [1..10000]
    where
        -- checks if a number is amicable (a /= n and sum equal each other)
        isAmi n = n == (sum . divisors) a && n /= a
            where a = sum . divisors $ n

        -- divisors generates a list of numbers that divide into n
        divisors n = f n [1..n-1]
            where
                f _ [] = []
                f n (x:xs)
                    | n `mod` x == 0 = x : f n xs
                    | otherwise = f n xs

-- (22) name scores
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
