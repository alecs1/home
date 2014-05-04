compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred1 :: (Num a, Ord a) => a -> Ordering
compareWithHundred1 = compare 100


--infix function
--equivalent to doing: (/10) 200
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)


--higher order functions:
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 _ [] _ = []
zipWith2 _ _ [] = []
zipWith2 f (x:xs) (y:ys) = f x y : zipWith2 f xs ys
