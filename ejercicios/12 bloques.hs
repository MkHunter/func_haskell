-- Pueden hacer la prueba de ayer con $500 de colecta a $30 la bolsa y el canje por 3 bolsas vacias
-- y otro con $300 de colecta a $50 la bolsa y el canje por 3 bolsas vacias
-- El ejemplo incluye  lo que vimos hoy sobre bloques do, let y conversion de tipos de dato

tostitos = do
   putStr "Hay promo de tostitos \n"
   putStr "trae tres bolsas vacias y te regalamos otra\n"
   putStr   "Cuanto dinero recolectaron?"
   --		  getLine lee datos por teclado	
   dinero1 <- getLine
   putStrLn "Cuanto cuesta la bolsa de tostitos?"
   costo1 <- getLine 
   putStrLn "Cuantas bolsas vacias ocupas para obtener una gratis?"
   cuantas1 <- getLine   
   --  let me permite definir variables locales en mi función   
   let dinero 	= cast dinero1 
   let costo 	= cast costo1 
   let cuantas = cast cuantas1
   let bolsas 	= paqPuedoComprar dinero costo
   
   --       $   sirve para concatenar varias acciones en una misma linea           
   --    show	me permite desplegar como String un dato numérico
   --	dinero1 y costo1 son String porque los leimos por teclado
   putStrLn $ "Recoleccion:" ++ dinero1 ++ " Bolsa:" ++ costo1 ++ " Completaron: " ++ show bolsas
    
   let otrasbolsas = bolsas `div` cuantas
   let masBolsas =  paqExtra otrasbolsas bolsas cuantas  
   let extra = otrasbolsas + masBolsas
   -- 			sum   funcion de Haskell que suma todos los elementos de una lista
   let total = sum [bolsas, otrasbolsas, masBolsas] 
   putStrLn $ "\n Bolsas extra:" ++ show extra   ++ "\n  TOTAL: " ++ show total
   
-- Me dice cuantas bolsas  puedo conseguir con el dinero que tengo actualmente
paqPuedoComprar :: Int -> Int -> Int   
paqPuedoComprar dinero costo = dinero `div` costo   

-- Me dice cuantas bolsas extra puedo conseguir con los paquetes canjeados y las originales
paqExtra otrasbolsas bolsas cuantas 
   -- En este primer caso considero las que no habia podido canjear y las que obtuve
   | bolsas `mod` cuantas > 0  = (otrasbolsas + (bolsas `mod` cuantas)) `div` cuantas
   -- En este caso solo considero las que habia canjeado
   | otherwise = otrasbolsas `div` cuantas
   
-- Hace un casting de la variable que recibe de String a Int
-- en Moodle en ChiCua11 hay otras conversiones de tipo de dato
cast :: String -> Int
cast x = (read:: String -> Int)   x