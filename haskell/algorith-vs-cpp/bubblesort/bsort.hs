module Main (
    main
) where

import System( getArgs)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  print (bSort (getNumbers contents))
  print (isSorted [1,2,0])
  print (bubble [1,2,0])
  print (bSort [1,2,0,-1])


getNumbers :: String -> [Double]
getNumbers contents = map strToDouble (words contents)

strToDouble :: String -> Double
strToDouble str = read str :: Double

bSort :: [Double] -> [Double]
bSort list
  | isSorted list = list
  | otherwise = bSort(bubble list)

bubble :: [Double] -> [Double]
bubble (x1:x2:xs)
  | x1 > x2   = x2:(bubble (x1:xs))
  | otherwise = x1:(bubble (x2:xs))
bubble x = x

isSorted :: [Double] -> Bool
isSorted (x1:x2:xs)
  | x1 > x2 = False
  | otherwise = isSorted (x2:xs)
isSorted x = True


