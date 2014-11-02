module Main (
    main
) where

import System( getArgs )

main :: IO ()
main = do
  args <- getArgs
  print (generatePrimes (read (head args)::Int))


generatePrimes :: Int -> [Int]
generatePrimes x
  | x < 2  = []
  | x == 2 = [2]
  | (x `mod` 2 == 1) = fillPrimes x
  | (x `mod` 2 == 0) = fillPrimes (x-1)


-- fill all primes; x need to be >= 3 and odd
fillPrimes :: Int -> [Int]
fillPrimes x
  | (x == 3) = [2,3]
  | (isPrime x previousPrimes) = previousPrimes ++ [x]
  | otherwise = previousPrimes
  where
        previousPrimes = fillPrimes (x - 2)


-- search if a number is prime by dividing it to primes before it
isPrime :: Int -> [Int] -> Bool
isPrime number primes
  | (null primes) = True
  | (((head primes) * (head primes)) > number) = True
  | ((number `mod` (head primes)) == 0) = False
  | otherwise = isPrime number (tail primes)
