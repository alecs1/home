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

getNextGen ::


