cuadrado :: Num a => a -> a
cuadrado n = n*n

-- Recibe Dos puntos y retorna el número de saltos
saltos :: (Int,Int) -> (Int,Int) -> Int
saltos (x1,y1) (x2,y2) = abs(abs(x1+y1)-abs(x2+y2))

-- Recibe Dos puntos y retorna su punto intermedio
medio :: (Double,Double) -> (Double,Double) -> (Double,Double)
medio (x1,y1) (x2,y2) = (x3,y3)
    where x3 = (x1+x2)/2
          y3 = (y1+y2)/2

-- Recibe un número y retorna sus números consecutivos en una tupla
predsuc :: Int -> (Int,Int)
predsuc central = (central-1,central+1)

prim :: (a,b) -> a
prim (x,y) = x
sec  :: (a,b) -> b
sec (x,y) = y

-- Llamar una función dentro de otra
sumar :: (Int,Int) -> Int
sumar valores = prim valores + prim valores

-- Función que retorne los dos mayores de tres números
mayor :: (Int,Int,Int) -> (Int,Int)
mayor (a,b,c)
    | m1 /= m2 = (m1,m2)
    | otherwise = (m1,m3)
    where m1 = max a b
          m2 = max b c
          m3 = max a c

{- tipo de datos enumerado para representar los días de la semana -}
data DiaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving (Eq, Ord, Enum, Read, Show, Bounded)
{- funciones booleanas que reconocen días festivos y laborales -}
diaLaborable :: DiaSemana -> Bool
diaLaborable d = Lunes <= d && d <= Viernes
diaFestivo :: DiaSemana -> Bool
diaFestivo d = d==Sabado || d == Domingo