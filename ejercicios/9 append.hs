-- aplaude 2 [5,4,6,2,8,1,9]
aplaude:: Integer -> [Integer] -> [Integer]
aplaude 0 xs = xs
aplaude n xs = aplaude (n-1) (zipWith (+) mequedo meregalaron) -- Suma de listas
    where   mequedo     = map (\x -> div (x + rem x 2) 2) xs   -- Division entre 2 de lista e incremento si es impar
            meregalaron = (++) [last mequedo] (init mequedo)   -- appdend ultimo elemento y resto de cadena

-- Pruebas
-- map (\x ->(rem x 2 == 0)) [1,4,5,8,9]
-- map (+2) $ filter (>0) [-10..120]
-- map (+1) $ filter (odd) [1,4,5,8,9]