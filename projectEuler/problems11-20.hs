import Data.List
import Data.Char (digitToInt)
import Data.Array
import Data.Ord (comparing)
import System.IO

-- (11) largest product in a grid
prob11 :: IO ()
prob11 = do
    handle <- openFile "textFiles/p11.txt" ReadMode
    contents <- hGetContents handle
    -- grid creates a list of integer lists [[int]] from text file
    let grid = map toInt . toSublist . filter (/='\n') . filter (/=' ') $ contents
        -- toInt changes two characters from into an integer  
        toInt [] = []
        toInt (a:b:xs) = (read (a:b:[]) :: Integer) : toInt xs

        -- toSublist changes grid into list of sublists/string
        toSublist xs = f xs 0 []
            where
                f _ 800 ys = ys
                f xs index ys = f xs (index+40) (take 40 (drop index xs):ys)

        -- store max product in specific directions
        vert = vertical grid 0 0 0
        horiz = horizontal grid 0 0 0
        diagR = diagonalR grid 0 0 0
        diagL = diagonalL grid 0 19 0

        -- vertical finds max product of 4 adjacent digits in up/down direction
        vertical _ 17 _ max = max
        vertical xs row 20 max = vertical xs (row+1) 0 max
        vertical xs row col max
            | vertProd > max = vertical xs row (col+1) vertProd
            | otherwise = vertical xs row (col+1) max
                where
                    vertProd = xs !! row !! col * 
                               xs !! (row+1) !! col * 
                               xs !! (row+2) !! col * 
                               xs !! (row+3) !! col

        -- horizontal finds max product of 4 adjacent digits in left/right direction
        horizontal _ 20 0 max = max
        horizontal xs row 17 max = horizontal xs (row+1) 0 max
        horizontal xs row col max
            | horizProd > max = horizontal xs row (col+1) horizProd
            | otherwise = horizontal xs row (col+1) max
                where
                    horizProd = product $ take 4 (drop col (xs !! row))
        
        -- diagonalR finds max product of 4 adjacent digits going 
        -- from top-left to bottom-right diagonal direction
        diagonalR _ 17 _ max = max
        diagonalR xs row 17 max = diagonalR xs (row+1) 0 max
        diagonalR xs row col max
            | diagProd > max = diagonalR xs row (col+1) diagProd
            | otherwise = diagonalR xs row (col+1) max
                where
                    diagProd = xs !! row !! col * 
                               xs !! (row+1) !! (col+1) * 
                               xs !! (row+2) !! (col+2) * 
                               xs !! (row+3) !! (col+3)

        -- diagonalL finds max product of 4 adjacent digits 
        -- from top-right to bottom-left diagonal direction
        diagonalL _ 17 _ max = max
        diagonalL xs row 2 max = diagonalL xs (row+1) 19 max
        diagonalL xs row col max
            | diagProd > max = diagonalL xs row (col-1) diagProd
            | otherwise = diagonalL xs row (col-1) max
                where
                    diagProd = xs !! row !! col * 
                               xs !! (row+1) !! (col-1) * 
                               xs !! (row+2) !! (col-2) * 
                               xs !! (row+3) !! (col-3)

    print $ max vert $ max horiz $ max diagL diagR 
    hClose handle

-- (12) highly divisible triangular numbers
prob12 :: Integer
prob12 = head $ filter ((>500) . factorLength) triNums
    where
        -- factorLength calculates how many factors exist for a number n
        -- group exponents of primeFactors, add 1 (succ) to each number of primes, 
        -- then multiply to get total number of factors/divisors
        factorLength n = product $ map (succ . length) $ group $ primeFactors n

        -- triNums generates an infinite list of triangular numbers
        triNums = f 1 2
            where f a b = a : f (a+b) (b+1)

primes :: [Integer]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors :: Integer -> [Integer]
primeFactors n = f n primes
    where
        f n (x:xs)
            | x*x > n = [n]
            | n `mod` x == 0 = x : f (n `div` x) (x:xs)
            | otherwise = f n xs

-- (13) large sum
prob13 :: IO ()
prob13 = do
    handle <- openFile "textFiles/p13.txt" ReadMode
    contents <- hGetContents handle
    let numList = map (\x -> read x :: Integer) $ lines contents
    print $ (take 10 . show . sum) numList
    hClose handle

-- (14) longest collatz sequence
prob14 :: (Integer,Integer) -- first number is answer, second is collatz sequence length
prob14 = maximumBy (comparing snd) $ assocs $ collatzArray
    where
        -- use array to memoize previously computed collatz lengths
        -- example below:
        -- collatz sequence of 13 is [40,20,10,5,16,8,4,2,1]
        -- collatz sequence of 40 is [20,10,5,16,8,4,2,1]
        -- notice collatz sequence of 40 repeats sequence of 13
        collatzArray = listArray (1,1000000) $ 1 : map collatzLength [2..1000000]

        -- computes the collatz sequence length of a number n
        collatzLength n =
            -- if length has already been computed & stored in array, use that value and add 1
            if collatzNum <= 1000000
                then 1 + collatzArray ! collatzNum
                else 1 + collatzLength collatzNum
            where
                collatzNum = if even n then n `div` 2 else 3*n+1

-- (15) lattice paths
-- in 20x20 grid, to get to bottom-right corner, you have to take 40 steps
-- you must go down 20 steps and to the right 20 steps (order of right direction does not matter)
-- therefore, use combinations C(40,20) = 40! / (20!)(40-20)!
prob15 :: Integer
prob15 = factorial 40 `div` (factorial 20 * factorial (40-20))
    where factorial n = product [2..n]

-- (16) power digit sum
prob16 :: Int
prob16 = sum . map digitToInt . show $ 2^1000

-- (17) number letter counts
prob17 :: Int
prob17 = sum $ map (length . numLetterCount) [1..1000]
    where
        numLetterCount n = f (show n) ""
            where
                f "" xs = xs
                f n xs
                    | length n == 1 = xs ++ singleDigit ! (digitToInt $ head n)
                    | length n == 2 && (read n :: Int) > 10 && (read n :: Int) < 20 = xs ++ special ! (digitToInt $ last n)
                    | length n == 2 = f (tail n) (xs ++ doubleDigit ! (digitToInt $ head n))
                    | length n == 3 && (read n :: Int) `mod` 100 == 0 = f (tail n) (xs ++ singleDigit ! (digitToInt $ head n) ++ "hundred")
                    | length n == 3 = f (tail n) (xs ++ singleDigit ! (digitToInt $ head n) ++ "hundredand")
                    | (read n :: Int) == 1000 = "onethousand"
                    | otherwise = error "Number out of range [1..1000]"
                
                singleDigit = listArray (0,9) ["","one","two","three","four","five","six","seven","eight","nine"]
                doubleDigit = listArray (0,9) ["", "ten","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
                special = listArray (1,9) ["eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]


-- (19) counting sundays
-- 0 = Sunday, 1 = Monday, 2 = Tuesday, 3 = Wednesday, etc.
prob19 :: Int
prob19 = length $ filter (==0) . drop 12 . take 1212 $ since1900
    where
        since1900 = scanl nextMonth 1 . concat $ generateDays
            where  
                generateDays = replicate 4 nonLeap ++ cycle (leap : replicate 3 nonLeap)
                
                -- nextMonth determines what day the month begins on
                -- example: January 1, 1900 is Monday, when using (nextMonth 1 31 = 4)
                -- which means February starts on a Thursday
                nextMonth x y = (x + y) `mod` 7
                nonLeap = [31,28,31,30,31,30,31,31,30,31,30,31]
                leap = 31 : 29 : drop 2 nonLeap

-- (20) factorial digit sum
prob20 :: Int
prob20 = sum $ map digitToInt $ show $ product [2..100]
