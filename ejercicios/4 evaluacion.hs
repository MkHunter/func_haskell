{-
Realiza las siguientes funciones en Haskell, ejecútalas y anota el resultado de cada ejecución.

    1.       Dadas las dimensiones ancho y alto de dos terrenos rectangulares, 
    definir el área del terreno mayor.
-}
amayor :: (Double,Double)->(Double,Double)->Double
amayor (w1,h1) (w2,h2) = may
    where a1 = w1*h1
          a2 = w2*h2
          may = max a1 a2
          
{-
    2.       En el estado de Sinaloa el periodo de siembra de hortalizas es entre 
    los meses de noviembre a enero, fuera de esos periodos la siembra no es exitosa. 
    Definir una función en Haskell que determine si la siembra tendrá éxito teniendo 
    como entrada el mes. El resultado debe ser un valor booleano.
    ``
-}
periohort :: Int -> Bool
periohort nmes = elem nmes [1,11,12]
 
{-
    3.       Hacer una función que retorne si una persona es niño (de 0 a 11 años), 
    joven (de 12 a 18 años), adulto (de 19 a 50) y adulto mayor (de 50 o más años).
-}
between x y z = (x<=y)&&(y<=z)

redad :: Int -> String
redad edad
    |   between 0 edad 11  = "Es nino"
    |   between 12 edad 18 = "Es joven"
    |   between 19 edad 50 = "Es adulto"
    |   edad>50            = "Adulto mayor"
    |   otherwise          = "Edad imposible"
{-
    4.       Los autos que pagan tenencia son de modelos de diez años atrás al año actual. 
    Hacer una función que dado el año actual y el modelo del auto indique si ese auto 
    pagará tenencia o no.
-}
ptenen :: Int -> Int -> Bool
ptenen actY modelo = between (actY-10) modelo actY

{-
    5.       En el nuevo modelo de competencias los alumnos son calificados como 
    “Sobresalientes“ si obtienen calificación de 95 a 100, 
    “Altamente Competentes” si su calificación es entre 90 y 94, 
    “Competente” si su calificación es 80 a 89, 
    “Suficiente” si obtiene entre 70 y 79, 
    “No Competente” en caso que su calificación sea menor a 70. 
    
    Hacer una función en Haskell que reciba el puntaje y retorne como 
    salida la leyenda de competencia correspondiente.
-}

califica :: Int -> String
califica puntuacion
    |   between 95 puntuacion 100 = "Sobresalientes"
    |   between 90 puntuacion 94  = "Altamente Competentes"
    |   between 80 puntuacion 89  = "Competente"
    |   between 70 puntuacion 79  = "Suficiente"
    |   between 0  puntuacion 69  = "No Competente"
    |   otherwise                 = "Calificacion fuera de rango"