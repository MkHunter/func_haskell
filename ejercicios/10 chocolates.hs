import Data.List
{- 
ParcialHaskell :: -> PasasExamen-> EntregasProyecto ->  EntregasteTodoProlog -> PasasPLF

1) Definir la función puntosGanados la cual recibe la elección de dos niños, 
   que eligen entre comprar (D) Dulces o (C) Chocolates, devuelve el importe 
   a pagar por cada niño. 
   Si los dos eligen dulces, se los dan a $5, si eligen chocolates son $1, 
   si cada uno elige algo diferente el precio cambia según lo que elijan.

  |   D   |  C
D | (3,3) | (0,5)
C | (5,0) | (1,1)

 (D,D)
 (C,C)
 (C,D)
 (D,C)
-}
data Caramelo = D | C
    deriving(Eq,Ord,Enum,Read,Show,Bounded)

elecciones = [(x,y) | x <- [D,C], y <- [D,C]]
getcosto (Just n) = [(3,3),(0,5),(5,0),(1,1)]!!n

{-
2) Definir la función unirListas :: [Elecciones]  -> [Elecciones]  -> [Elige], 
   la cual toma las listas de dulces pedidos por los 2 niños y devuelve una lista 
   emparejando las mismas.

Ejemplo: unirListas [C,C,D,D] [D,D,C,D] = [(C,D), (C,D), (D,C), (D,D)]
-}
unirListas:: [Caramelo] -> [Caramelo] -> [(Caramelo,Caramelo)]
unirListas = zip
{-
3) Definir la función gastoEnDulces :: [Elige]  -> [Gasto], 
   la cual toma una lista de pedidos de dulces emparejadas y devuelve una lista 
   con lo que van a pagar los niños en cada uno de sus pedidos.
  |   D   |  C
D | (3,3) | (0,5)
C | (5,0) | (1,1)

gastoEnDulces [(C,D), (C,D), (D,C), (D,D)]  >>> [ (5,0), (5,0), (0,5), (3,3) ] 
-}
costCombo:: (Caramelo,Caramelo) -> (Integer,Integer)
costCombo x = getcosto (elemIndex x elecciones)

gastoEnDulces:: [(Caramelo,Caramelo)] -> [(Integer,Integer)]
gastoEnDulces = map costCombo
{-
4) Definir la función gastosPersonales :: [Gasto]  -> ([Int], [Int]), 
   la cual toma una lista con lo que van a pagar los niños en sus pedidos 
   y devuelve un par de listas, donde cada componente es una lista 
   con los puntajes correspondientes a cada jugador.
 [(5,0), (5,0), (0,5), (3,3) ] >>>  ([5,5,0,3],[0,0,5,3])
-}
gastosPersonales :: [(Integer,Integer)] -> ([Integer],[Integer])
gastosPersonales xs = ([ x | (x,y) <- xs], [y | (x,y) <- xs])

{-
5) Definir la función sumarCobros :: [Int]  -> Int, la cual toma una lista 
   de lo que un niño ha tenido que pagar por cada una de las veces que ha ido 
   a la tienda y devuelve el importe total a pagar.
 [5,5,0,3] >>> 13
 [0,0,5,3] >>> 8
-}
sumarCobros :: [Integer]  -> Integer
sumarCobros = sum
{-
6) Definir la función todasLasCompras :: [Elecciones]  -> [Elecciones]  -> String, 
   la cual toma las listas de dulces que cada niño compró y devuelve una cadena que 
   nos dice quién fue el niño que gastó más en sus compras.
   [D,D,C,D] [C,C,D,C] >>> Niño 2 gastó más.
-}
todasLasCompras :: [Caramelo] -> [Caramelo] -> String
todasLasCompras xs ys
    | nino1 > nino2 = "Nino 1 gasto mas"
    | nino1 < nino2 = "Nino 2 gasto mas"
    | otherwise     = "Gastaron lo mismo"
    where (g1,g2) = gastosPersonales ( gastoEnDulces (unirListas xs ys))
          nino1  = sumarCobros g1
          nino2  = sumarCobros g2

{-
7) Definir la función jugar, que lea las elecciones de dos niños, el número de tiendas visitadas y nos dice quién gastó más >>>OPCIONAL>>>
-}
