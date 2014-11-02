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

-- first iteration, we suppose that the universe is bounded 0..n, 0..n
getNextGen :: Int -> Array (Int, Int) -> Array (Int, Int)
getNextGen inArray = outArray where
                    outArray = array((1, 1), (n, n))
                            ( [((i, j), 1) | neighCount inArray, i, j 


--size, input array, x, y
neighCount :: Int -> Array (Int, Int) -> Int -> Int -> Int
neighCount 



wavefront   :: Int -> Array(Int, Int) Int
wavefront n = a where
              a = array ((1,1), (n,n))
                  ([((1,j), 1) | j <- [1..n]] ++
                   [((i, 1), 1) | i <- [2..n]] ++
                   [((i, j), a!(i,j-1) + a!(i-1,j-1) + a!(i-1,j)) | i <- [2..n], j <- [2..n]])



