import Data.List
import Data.Char (digitToInt)
import Data.Array
import Data.Ord (comparing)
import System.IO

-- (11) largest product in a grid
prob11 :: Integer
prob11 = do
    handle <- openFile "prob11grid.txt" ReadMode
    contents <- hGetContents handle
    -- grid creates a list of integer lists [[int]] from text file
    let grid = map toInt . toSublist . filter (/='\n') . filter (/=' ') $ contents
        -- toInt changes two characters from into an integer  
        toInt [] = []
        toInt (a:b:xs) = (read (a:b:[]) :: Integer) : toInt xs

        -- toSubList changes grid into list of sublists/string
        toSublist xs = f 0 xs []
            where
                f 800 _ ys = reverse ys
                f index xs ys = f (index+40) xs (take 40 (drop index xs):ys)

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

diagonalL xs = maximum $ map fst $ f xs 0 19 []
    where
        f _ 17 _ ys = ys
        f xs row 2 ys = f xs (row+1) 19 ys
        f xs row col ys = f xs row (col-1) (diagProd:ys)
                where
                    diagProd = ((xs !! row !! col * 
                               xs !! (row+1) !! (col-1) * 
                               xs !! (row+2) !! (col-2) * 
                               xs !! (row+3) !! (col-3)),
                               [xs !! row !! col, xs !! (row+1) !! (col-1), xs !! (row+2) !! (col-2), xs !! (row+3) !! (col-3)])

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
