
-- PRODUCT
prod :: Num a => [a] -> a
prod [] = 1
prod ns = foldr (*) 1 ns
-- prod (n:ns) = n * prod ns
-- LENGTH
le :: [a] -> Int
le [] = 0
le (_:xs) = 1 + le xs
-- REVERSE
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]
-- ZIP
zi:: [a] -> [b] -> [(a,b)]
zi [] _ = []
zi _ [] = []
zi (x:xs) (y:ys) = (x,y) : zi xs ys
-- DROP
dro :: Int -> [a] -> [a]
dro 0 xs = xs
dro _ [] = []
dro n (_:xs) = dro (n-1) xs

-- NI IDEA
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ys)

-- QuickSort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a|a<-xs,a<=x]
        larger  = [b|b<-xs,b> x]