{-
Mover Izquierda y Derecha

comienza [0,0,0,1]

moviz [0,0,0,1] -> [1,0,1,0]

moviz [1,0,1,0] -> [2,0,0,1]

movde [2,0,0,1] -> [1,0,2,1]
...
comienza [1,1,2,1]

movde [1,1,2,1] -> [1,2,2,1]

movde [1,2,2,1] -> [1,1,4,1]

1. Moverlo al primer espacio libre
2. Checar si hay suma entre los consecutivos
3. Agregar un 1 en un espacio libre

El juego termina cuando ya no puedo realizar sumas y no existan números 0
-}
import Data.List
-- Si se mueve a la izquierda se concatenan los zeros a la derecha
moviz:: [Int] -> [Int]
moviz xs = (++) numeros zeros
    where (numeros,zeros) = mueve xs

-- Si se mueve a la derecha se concatenan los zeros a la izquierda
movde:: [Int] -> [Int]
movde xs = (++) zeros numeros
    where (numeros,zeros) = mueve xs

mueve:: [Int] -> ([Int],[Int])
mueve xs
    | len_dif < 1   = (numeros,zeroes)
    | otherwise = (numeros,1 : tail zeroes)
    where numeros = suma (filter (/=0) xs)     -- Ignorar los zeros y los manda a suma
          zeroes = map (const 0) [1..len]      -- Senera lista de 0 para espacios vacíos
          len_dif = length xs - length numeros -- Cálcula los espacios vacios

-- Intento con listas por comprehensión... (FALLIDO)
-- suma lista = [ a+b | a:b:_ <- tails lista, a == b]--[elemIndex x adivina |(x,y) <- lista, x == y]  n
-- supone que recibe una lista sin 0s
-- suma []
suma :: [Int] -> [Int]
suma [] = []
suma [x] = [x]
suma (a:b:xs) 
    | a == b = a*2 : suma xs
    | a /= b = a : suma (b:xs)

-- EJEMPLO: juega [0,0,0,0] []
-- NOTA: El valor de entrada del segundo parámetro es irrelevante, mientras sea tipo [Int]
juega arreglo anterior = do
    putStrLn $ "\n  Tu arreglo actuales:" ++ show arreglo ++ "\n  El arreglo anterior es: "++ show anterior
    putStr "\n Elige a dónde quieres mover [Izquierda|Derecha|Salir] = [0,1,Otro]: "
    opc <- getLine
    let opcion = cast opc

    let action | opcion == 0 = juega (moviz arreglo) arreglo
               | opcion == 1 = juega (movde arreglo) arreglo
               | otherwise   = putStr "\nTermina.\n"
    -- Realiza la validación de salida, si el resultado se repite al anterior, salgo
    let salta | arreglo /= anterior = action
              | arreglo == anterior = putStr "\nTermina.\n"
    salta

cast :: String -> Int
cast = read:: String -> Int