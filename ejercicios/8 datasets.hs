
-- tipo persona
type Nombre = String
type Profesion = String
type Nacimiento = Int
type Fallecimiento = Int
type Persona = (Nombre,Profesion,Nacimiento,Fallecimiento)
-- base de datos
personas :: [Persona]
personas = [
	("Cervantes","Literatura",1547,1616),("Velazquez","Pintura",1599,1660),
	("Picasso","Pintura",1881,1973),	("Beethoven","Musica",1770,1823),
	("Poincare","Ciencia",1854,1912),	("Quevedo","Literatura",1580,1654),
	("Goya","Pintura",1746,1828),		("Einstein","Ciencia",1879,1955),
	("Mozart","Musica",1756,1791),		("Botticelli","Pintura",1445,1510),
	("Borromini","Arquitectura",1599,1667),("Bach","Musica",1685,1750)]

-- funci�n que proporciona la lista de la nombres de personas de la BD
-- nombres :: [(String,String,Int,Int)] -> [String]
nombres :: [Nombre]
nombres = [nombre | (nombre,_,_,_) <- personas]

-- funci�n que retorna los nombres de las personas que son m�sicos
-- musicos :: [Persona] -> [Nombre]
nombresM :: [Nombre]
nombresM = [nombre | (nombre,prof,_,_) <- personas,prof=="Musica"]

-- funci�n que define los nombres de las personas cuya actividad es m
--ocupacion :: [Persona]-> Profesion -> [Nombre]
nombresProf :: Profesion -> [Nombre]
nombresProf prof = [nombre | (nombre,profesion,_,_) <- personas,prof==profesion]

-- Definir la funci�n vivas tal que (vivas bd a) es la lista de 
-- los nombres de las personas de la base de datos bd que 
-- estaban vivas en el a�o a
between x y z = (x<=y)&&(y<=z)

vivas :: Int -> [String]
vivas fecha = [nombre | (nombre,_,nac,fall) <- personas, between nac fecha fall]

