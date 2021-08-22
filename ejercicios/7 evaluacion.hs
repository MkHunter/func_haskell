-- VALIDA NUMERO DE TARJETA DE CREDITO
-- 
-- El objetivo de esta relación es estudiar un algoritmo para validar
-- algunos identificadores numéricos como los números de algunas tarjetas
-- de crédito; por ejemplo, las de tipo Visa o Master Card.  
--
-- El algoritmo que vamos a estudiar es el algoritmo de Luhn consistente
-- en aplicar los siguientes pasos a los dígitos del número de la
-- tarjeta.    
--    1. Se invierten los dígitos del número; por ejemplo, [9,4,5,5] se
--       transforma en [5,5,4,9].
--    2. Se duplican los dígitos que se encuentra en posiciones impares
--       (empezando a contar en 0); por ejemplo, [5,5,4,9] se transforma
--       en [5,10,4,18].
--    3. Se suman los dígitos de cada número; por ejemplo, [5,10,4,18]
--       se transforma en 5 + (1 + 0) + 4 + (1 + 8) = 19.
--    4. Si el último dígito de la suma es 0, el número es válido; y no
--       lo es, en caso contrario. 
--
-- A los números válidos, los llamaremos números de Luhn. 
 
-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    digitosInv :: Integer -> [Integer]
-- tal que (digitosInv n) es la lista de los dígitos del número n. en
-- orden inverso. Por ejemplo, 
--    digitosR 320274  ==  [4,7,2,0,2,3]
-- ---------------------------------------------------------------------
 
digitosInv :: Integer -> [Integer]
digitosInv n
    | n < 10 = [n]
    | otherwise = rem n 10 : digitosInv (div n 10)
 
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    doblePosImpar :: [Integer] -> [Integer]
-- tal que (doblePosImpar ns) es la lista obtenida doblando los
-- elementos en las posiciones impares (empezando a contar en cero y
-- dejando igual a los que están en posiciones pares. Por ejemplo,
--    doblePosImpar [4,9,5,5]    ==  [4,18,5,10] 
--    doblePosImpar [4,9,5,5,7]  ==  [4,18,5,10,7]
-- ---------------------------------------------------------------------
 
-- 1ª definición (por recursión)
doblePosImparR2 (x:y:resto) = x : 2*y : doblePosImparR2 resto
doblePosImparR2 x2          = x2
-- 2ª definición (por recursión)
--doblePosImparR :: [Integer] -> [Integer]
--doblePosImparR (a:b:restoTarjeta)
 
-- 3ª definición (por comprensión)
doblePosImparC :: [Integer] -> [Integer]
doblePosImparC xs = [ f posic valor | (posic, valor) <- zip [1..] xs]
    where f posic valor | even posic = 2*valor
                        | otherwise = valor

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    sumaDigitos :: [Integer] -> Integer
-- tal que (sumaDigitos ns) es la suma de los dígitos de ns. Por
-- ejemplo, 
--    sumaDigitos [10,5,18,4] = 1 + 0 + 5 + 1 + 8 + 4 =
--                            = 19
-- ---------------------------------------------------------------------
 
-- 1ª definición (por comprensión):
sumaDigitosC :: [Integer] -> Integer
sumaDigitosC ns = sum [sum (digitosInv n)  | n <- ns]
 
-- 2ª definición (por recursión):
sumaDigitosR :: [Integer] -> Integer
sumaDigitosR [] = 0
--sumaDigitosR (n:ns) = sum (digitosInv n) + sumaDigitosR ns
sumaDigitosR ns = foldr ((+) . sum . digitosInv) 0 ns
-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función  
--    ultimoDigito :: Integer -> Integer
-- tal que (ultimoDigito n) es el último dígito de n. Por ejemplo,
--    ultimoDigito 123 == 3
--    ultimoDigito   0 == 0
-- ---------------------------------------------------------------------
ultimoDigito :: Integer -> Integer
ultimoDigito n = rem n 10

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función 
--    luhn :: Integer -> Bool
-- tal que (luhn n) se verifica si n es un número de Luhn. Por ejemplo,
--    luhn 5594589764218858  ==  True
--    luhn 1234567898765432  ==  False
-- ---------------------------------------------------------------------
luhn :: Integer -> Bool
luhn n = ultimoDigito(sumaDigitosR(doblePosImparR2 (digitosInv n))) == 0