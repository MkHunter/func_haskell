--              López Malacón Miguel Ernesto
import Data.List
{-
Función 1:
mostrarUnaBroma:: ( catafixiaQueEligeElCuate, CatafixiaDondeEstaElCarrro)  -> catafixiadeBroma
mostrarUnaBroma(3,1)  :. 2
mostrarUnaBroma(2,1)  :. 3
-}

mostrarUnaBroma :: (Integer,Integer) -> Integer
mostrarUnaBroma (a,b) = head [x |x <- [1..3], x `notElem` [a,b]]
{-
    | a == b = caso2
    where Just caso2 = [x |x <- [1..3], x `notElem` [a,b]]!!rng
          rng = randomRIO(0,1)
-}

--castRNG >>= :: IO a -> (a -> IO b) -> IO b
{-
Función 2:
tomaDecision:: (CatafixiaElegidaPorCuate, CatafixiaConElCarro, DecisiónDelCuate) -> RespuestaPerdióGanó
tomaDecision (2,1,"no") -> Perdió
tomaDecision (3,1,"Si") -> Perdió
tomaDecision (1,1,"Si") -> Ganó
-}
tomaDecision:: (Integer, Integer, String) -> String
tomaDecision (opc_elegida, opc_ganadora, decision)
    |   opc_elegida == opc_ganadora && decision == "si" = "Ganador"
    |   otherwise                                       = "Perdedor"


juega carro = do
    putStr "\n Elige to catafixia del 1-3. ej. 2: "
    elecc <- getLine
    let eleccion = cast elecc
    let broma    = mostrarUnaBroma(eleccion,carro)
    
    putStrLn $ "\n Deseas permanecer con la catafixia que elegiste (si) o cambiar por la opción " ++ show broma ++ " (no)?: "
    cambias <- getLine
    -- Realiza la validación de salida, si el resultado predicho es correcto
    let salta | cambias == "si" = putStrLn $ "\n  Tu eres un " ++ show (tomaDecision (eleccion,carro,cambias))
              | cambias == "no" = juega carro
    salta

cast :: String -> Integer
cast = read:: String -> Integer