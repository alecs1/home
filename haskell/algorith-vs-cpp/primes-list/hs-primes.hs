-- generate primes up to the argument
-- try to match the algorithm and the performance of the imperative version
-- which uses a list of numbers which are already computed as primes

-- skip main, just write the function first
-- module Main ( main ) where


import Data.Char (digitToInt)
import System( getArgs )

asInt :: String -> Int
asInt xs = loop 0 xs

loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

main :: IO ()
main = do
  args <- getArgs
  print (generatePrimes2 (asInt (head args)))

-- argToInt :: String -> Int


-- generatePrimes :: Int -> [Int]
-- generatePrimes x
--   | x < 2  = []
--   | x == 2 = [2]
--   | otherwise = fillPrimes [2] 3 x
--
-- fillPrimes :: [Int] -> Int -> Int -> [Int]
-- fillPrimes primes crt lastNumber =
--   case isPrime crt primes of
--          True -> crt : fillPrimes primes crt+1 lastNumber
--          False -> fillPrimes primes crt+1 lastNumber
--          _ -> []
--
-- -- Fill this function in
-- isPrime :: [int] -> Int -> Bool
-- isPrime list number = True


-- simpler tries
generatePrimes2 :: Int -> [Int]
generatePrimes2 x
  | x < 2 = []
  | otherwise = 2 : filter isPrime2 [3, 5 .. x]

isPrime2 :: Int -> Bool
isPrime2 x
  | x == 2      = True
  | otherwise   = not (hasOddDivisor x 3)

hasOddDivisor :: Int -> Int -> Bool
hasOddDivisor x divisor
  | ((x `mod` divisor == 0) && (divisor <= (x `div` 2))) = True
  | (divisor < (x `div` 2)) = hasOddDivisor x (divisor + 2)
  | otherwise = False


