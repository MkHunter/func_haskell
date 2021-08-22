{-
DEFINIR UNA VARIABLE  [1,2,3,4]
 - Generar una lista de cuatro números
 - No deben repetirse
 - Solo numeros 1-6
 
FUNCIONES
 1a- Decir cuantos números son correctos (alguno de nlos números que teclee 
     está en la misma poscicion que el de la variable definida)
 
 [1,2,3,4] variable
 [6,5,3,4] datoTecleado  -> Correctos 2
 
 [1,2,3,4] variable
 [3,2,5,1] datoTecleado  -> Correctos 1
 -}

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

 {-
 3a - Cuantos números son incorrectos : No estan en la lista original
 
 [1,2,3,4] variable
 [6,5,3,4] datoTecleado  -> Incorrectos 2
 
 [1,2,3,4] variable
 [3,2,5,1] datoTecleado  -> Incorrectos  1
  
 [1,2,3,4] variable
 [1,2,3,4] datoTecleado  -> Incorrectos  
 
 [1,2,3,4] variable
 [2,3,4,1] datoTecleado  -> Incorrectos  0
 -}