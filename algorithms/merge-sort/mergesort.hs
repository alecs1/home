-- test array [19, 0, -1, 5, 7, -39, 2]

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSort2 (split xs)


mergeSort2 :: Ord a => ([a], [a]) -> [a]
mergeSort2 ([x], []) = [x]
mergeSort2 ([], [y]) = [y]
mergeSort2 ((x:[]), (y:[])) =
    if (x<=y) 
        then [x,y]
        else [y,x]
mergeSort2 (xs, ys) = merge (mergeSort2 (xL, xR)) (mergeSort2 (yL, yR)) 
                         where (xL, xR) = split xs
                               (yL, yR) = split ys




split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:[]) = ([x], [])
split (x:y:xy) = ((x:xs), (y:ys)) where
                         (xs, ys) = split xy


merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge (x:xs) [] = x:xs
merge [] (y:ys) = y:ys
merge (x:xs) (y:ys) = 
    if x <= y
       then x:merge xs (y:ys)
       else y:merge (x:xs) ys




