--Editado el 08-05-2020

import Data.List
{- Definir una función que nos diga qué meses corresponden  a cada estación
 ejemplo: Primavera == Marzo, Abril, Mayo
-}
between x y z = (x<=y)&&(y<=z)

data Mes = Enero|Febrero|Marzo|Abril|Mayo|Junio|Julio|Agosto|Septiembre|Octubre|Noviembre|Diciembre
    deriving(Eq,Ord,Enum,Read,Show,Bounded)

data Estacion = Primavera|Verano|Otono|Invierno
    deriving(Eq,Ord,Enum,Read,Show,Bounded)

estacion :: Mes -> Estacion
estacion mes
    | between Marzo mes Mayo                          = Primavera
    | between Junio mes Agosto                        = Verano
    | between Septiembre mes Noviembre                = Otono
    | between Enero mes Febrero || mes == Diciembre   = Invierno

{- Definir la función mcd, tal que (mcd a b) es el máximo común divisor de 
   a y b calculado mediante el algoritmo de Euclides. Por ejemplo, mcd 30 45 == 15
-}

-- Obtener los factores de un número
factores:: Integer -> [Integer]
factores num = [x | x <- [1..num],mod num x == 0]

-- Se usa la función intersect de Data.List
mcd :: Integer -> Integer -> Integer
mcd a b = maximum (factores a  `intersect` factores b) 

-- Calcula el factorial de un numero usando: a) guardas y b) Patrones
fact :: Integer ->  Integer
fact x
    |   x == 0 = 1
    |   otherwise = x * fact (x-1)
    
-- Decir hacia donde miras si giras 90 grados
girar90 :: Integer -> Integer
girar90 grados = grados + 90

-- Decir que color sigue en el semaforo a partir del color actual
data Color = Rojo|Amarillo|Verde
    deriving(Eq,Ord,Enum,Read,Show,Bounded)

semaforo :: Color -> Color
semaforo color
    | color == Verde = Rojo
    | otherwise = succ color

-- Dice si el parametro b es multiplo del parametro a
multiploDe :: Integer -> Integer -> Bool 
multiploDe a b = mod a b == 0

-- Retorna las raíces de una funcion en base a sus coeficientes
raices :: Float ->  Float ->    Float -> (Float, Float)
raices a b c = (raiz1,raiz2)
    where raiz1 = (-b + parcial)/(2*a)
          raiz2 = (-b - parcial)/(2*a)
          parcial = sqrt (b**2 - 4*a*c)

-- Funcion que reciba un numero y retorne cero
cero :: Integer -> Integer
cero _ = 0

-- Recibe la edad de una persona y le dice si puede votar
votar :: Int -> Bool
votar edad = edad > 17

-- Dice si se presenta examen, estan excentos solo los 100 
excenta :: (Eq a, Num a) => a -> String
excenta calificacion
    |   calificacion == 100 = "Excentaste"
    |   otherwise           = "No excentaste"

-- Recibe un número y nos dice si esta en el rango 0-9
rango09:: Integer -> Bool
rango09 valor = between 0 valor 9
-- recibe un número y nos regresa en una tupla el previo y siguiente a dicho número
prevsig:: Integer -> (Integer,Integer)
prevsig valor = (valor-1,valor+1)
-- Definir una funcion que recibe dos valores booleanos y simula el resultado del AND LOGICO
andL:: Bool -> Bool -> Bool
andL cond1 cond2 = cond1&&cond2
-- Recibe un numero y nos retorna su valor absoluto
abso:: Integer -> Integer
abso = abs
-- Recibe un numero y nos dice que signo tiene
signo:: Integer -> String
signo valor
    |   valor < 0 = "Negativo"
    | otherwise   = "Positivo"