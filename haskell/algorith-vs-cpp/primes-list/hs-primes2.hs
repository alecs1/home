module Main (
    main
) where

import System( getArgs )

main :: IO ()
main = do
  args <- getArgs
  print (generatePrimes (read (head args)::Int))
--  print (generatePrimes 25)


generatePrimes :: Int -> [Int]
generatePrimes x
  | x < 2  = []
  | otherwise = fillPrimes [2] 3 x

-- assume crt is an odd number
fillPrimes :: [Int] -> Int -> Int -> [Int]
fillPrimes primes crt lastNumber
  | (crt > lastNumber) = primes
  | (isPrime crt primes) = fillPrimes (primes++[crt]) (crt+2) lastNumber
  | otherwise = fillPrimes primes (crt+2) lastNumber

-- search if a number is prime by dividing it to primes before it
isPrime :: Int -> [Int] -> Bool
isPrime number primes
  | (null primes) = True
  | (((head primes) * (head primes)) > number) = True
  | ((number `mod` (head primes)) == 0) = False
  | otherwise = isPrime number (tail primes)
