
entre:: Double -> Double -> Double -> Bool
entre valor inferior superior = (valor >= inferior)&&(valor <= superior)

signo:: Int -> String
signo numero
    | numero > 0 = "Positivo"
    | numero < 0 = "Negativo"
    | otherwise = "Cero, no tiene signo"

-- Recibe un número y retorna el signo del número
-- Función con patrones que retorne 
colores :: String -> String
colores color
    | color == "rojo"  = "Manzana, fresa"
    | color == "azul"  = "Cielo, pitufos"
    | color == "negro" = "Noche, cabello de blanca nieves"
    | otherwise = "Color no encontrado"

-- Utilizando patrones
coloresP :: String -> String
coloresP "rojo"  = "Manzana, fresa"
coloresP "azul"  = "Cielo, pitufos"
coloresP "negro" = "Noche, cabello de blanca nieves"
coloresP _       = "Color no encontrado"

-- AND LÓGICO
andLogico :: Bool -> Bool -> Bool
andLogico True True = True
andLogico _ _ = False

-- Indice de masa corporal
imc :: Double -> Double -> String
imc peso altura
    | myIMC < 18.5                       = "Bajo de peso"
    | (myIMC >= 18.59) && (imc <= 24.59) = "Peso normal"
    | imc >= 25.9 && imc <= 29.9         = "Sobrepeso"
    where myIMC = peso / (altura^2)