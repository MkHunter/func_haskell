compara3 :: Int -> Int -> Int -> Bool
compara3     a     b      c    = (a==b) && (b==c)

doble :: Int -> Int
doble a = a*2

absoluto :: Int -> Int
absoluto a = if a<0
    then a * (-1)
    else a

absoluto2 :: Int -> Int
absoluto2 a
    | a<0 = a*(-1)
    | otherwise = a

mayor2 :: Int -> Int -> Int
mayor2 a b
    | a > b = a
    | otherwise = b

mayor3 :: Int -> Int -> Int -> Int
mayor3 a b c
    | (a > b)&&(a > c) = a
    | (b > a)&&(b > c) = b
    | otherwise = c
