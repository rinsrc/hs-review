import Data.List
import Data.Array
import Data.Ord (comparing)

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
