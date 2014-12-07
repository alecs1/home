-- matrix addition, matrix represented as list of lists
mAdd :: Num a => [[a]] -> [[a]] -> [[a]]
mAdd m n = zipWith (zipWith (+)) m n


-- print mAdd [[1,0,0], [0,1,0], [0,0,1]] [[0,0,1],[0,1,0],[1,0,0]]

