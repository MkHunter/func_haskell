import Data.List
import System.Random (randomRIO)
{-
DEFINIR UNA VARIABLE  [1,2,3,4]
 - Generar una lista de cuatro números
 - No deben repetirse
 - Solo numeros 1-6
-}
-- randomList lenght min max
-- randomList 4 1 6
-- Leer lista de números haskell
randomList :: Integer -> Integer -> Integer -> IO([Integer])
randomList 0 _ _ = return []
randomList n min_l max_l = do
  r  <- randomRIO (min_l,max_l)
  rs <- randomList (n-1) min_l max_l
  return (r:rs)

{-
FUNCIONES
 1a- Decir cuantos números son correctos (alguno de nlos números que teclee 
     está en la misma poscicion que el de la variable definida)
 
 [1,2,3,4] variable
 [6,5,3,4] datoTecleado  -> Correctos 2

 [1,2,3,4] variable
 [3,2,5,1] datoTecleado  -> Correctos 1
-}
-- correctos [1,2,3,4] [6,5,3,4]
-- correctos [1,2,3,4] [3,2,5,1]
correctos:: [Int] -> [Int] -> Int
correctos adivina intento = length [elemIndex x adivina |(x,y) <- zip adivina intento, x == y]

--indxcorrect:: [Int] -> [Int] -> [Int]
{-
 2a - Decir cuantos números si están en el original, 
      pero no estan en la posicion que deben estar, ignorar a los correctos

 [1,2,3,4] variable
 [6,5,3,4] datoTecleado  -> PosicIncorrecta 0
 
 [1,2,3,4] variable
 [3,2,5,1] datoTecleado  -> PosicIncorrecta  2
 
 [1,2,3,4] variable
 [1,2,3,4] datoTecleado  -> PosicIncorrecta  0
 -}
-- malacom [1,2,3,4] [6,5,3,4]
-- malacom [1,2,3,4] [3,2,5,1]
-- malacom [1,2,3,4] [1,2,3,4]
malacom:: [Int] -> [Int] -> Int
malacom adivina intento = length [elemIndex x adivina |(x,y) <- zip adivina intento, x /= y, x `elem` intento]

 {-
 3a - Cuantos números son incorrectos : No estan en la lista original
 [1,2,3,4] variable
 [6,5,3,4] datoTecleado  -> Incorrectos 2
 
 [1,2,3,4] variable
 [3,2,5,1] datoTecleado  -> Incorrectos  1
  
 [1,2,3,4] variable
 [1,2,3,4] datoTecleado  -> Incorrectos  0
 
 [1,2,3,4] variable
 [2,3,4,1] datoTecleado  -> Incorrectos  0
 -}
-- incorrectos [1,2,3,4] [6,5,3,4]
-- incorrectos [1,2,3,4] [3,2,5,1]
-- incorrectos [1,2,3,4] [1,2,3,4]
-- incorrectos [1,2,3,4] [2,3,4,1]
incorrectos:: [Int] -> [Int] -> Int
incorrectos adivina intento = length [elemIndex x adivina |(x,y) <- zip adivina intento, x `notElem` intento]

{-              PRUEBA
PEDICCION | Correcto | Mal acom. | Incorrecto |
[1,2,3,4] | 0        | 3         | 1          |
[3,4,6,5] | 1        | 1         | 2          |
[6,5,4,3] | 0        | 2         | 2          |
[6,1,3,4] | 0        | 3         | 1          |
[4,3,1,5] | 1        | 1         | 2          |
[4,5,1,6] | 1        | 1         | 2          |

RESPUESTA = [3,6,1,2]
-}
-- EJEMPLO: juega [3,6,1,2]
-- NOTA: El valor de puede ser generado utilizando la función randomList. ej. randomList 4 1 6
-- IMAGEN DE CORRIDA: ;P https://ibb.co/LZ14qDD
juega respuesta = do
    putStr "\n Escribe tu nueva Predicción. ej. [1,2,3,4]: "
    listaLine <- getLine
    let lista = read listaLine :: [Int]

    putStrLn $ "\n  Correctos:" ++ show (correctos respuesta lista) ++ "\n  Mal acomodados: " ++ show (malacom respuesta lista) ++ "\n  Incorrectos: "++ show (incorrectos respuesta lista)
    -- Realiza la validación de salida, si el resultado predicho es correcto
    let salta | lista /= respuesta = juega respuesta
              | lista == respuesta = putStr "\n Respuesta correcta, Felicidades, tienes 100.\n"
    salta