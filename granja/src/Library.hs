module Library where
import PdePreludat
import Data.Foldable (Foldable(fold, length))
import System.Posix.Internals (fileType)
import qualified Control.Applicative as Haskell

--COMIDAS
{-

data Persona = Persona {
  colesterol :: Number,
  peso :: Number
} deriving Show

type Comida = Persona -> Persona

ensalada :: Number -> Comida
ensalada kilos persona = persona {
  peso = peso persona + (kilos / 2)
}
hamburguesa :: [String] -> Comida
hamburguesa ingredientes persona = persona {
  colesterol = ((*1.5).colesterol) persona,
  peso = peso persona + ((*3).length) ingredientes
}

palta :: Comida
palta persona = persona{
    peso = ((+2).peso) persona
} 

almuerzo :: [Comida]
almuerzo = [ensalada 1, hamburguesa ["cheddar", "bacon"], palta, ensalada 3]

almorzar :: Comida
almorzar persona = foldr ($) persona almuerzo
-}

--GRANJA
data Visitas = Visitas{
    dias :: Number,
    costo :: Number
} deriving (Show)

data Animal = Animal {
    nombre :: String,
    tipo :: String,
    peso :: Number,
    edad :: Number,
    estaEnfermo :: Bool,
    visitas :: [Visitas]
} deriving (Show)

oveja = Animal{
  nombre = "Oscar",
  tipo = "oveja",
  peso = 50,
  edad = 15,
  estaEnfermo = False,
  visitas = []
}


--1
laPasoMal :: Animal -> Bool
laPasoMal animal = (any ((>30).dias).visitas) animal

nombreFalopa :: Animal -> Bool
nombreFalopa animal = ((=='i').last.nombre) animal


--2
type Actividad = Animal -> Animal

engorde :: Number -> Actividad
engorde kilos animal = animal{
  peso = peso animal + min 5 (kilos/2)
}

revision :: Number -> Number -> Actividad
revision diasRecuperacion costoRecuperacion animal
  | estaEnfermo animal = engorde 2 animal{
    visitas = visitas animal ++ [Visitas diasRecuperacion costoRecuperacion]
    --peso = (peso.engorde 2) animal
  }
  | otherwise = animal

modificarPeso :: Number -> Actividad
modificarPeso kilos animal = animal{
  peso = ((+kilos).peso) animal
}

cumpleaños :: Actividad
cumpleaños animal = modificarPeso (-1) animal{
  edad = ((+1).edad) animal
}

chequeoPeso :: Number -> Actividad
chequeoPeso kilos animal = animal{
    estaEnfermo = ((<=kilos).peso) animal
  }


--3
type Proceso = [Actividad]

proceso :: Proceso
proceso = [engorde 2, revision 4 5, cumpleaños, chequeoPeso 10]

aplicarProceso :: [Actividad] -> Actividad
aplicarProceso actividades animal = foldr ($) animal actividades

{-
> aplicarProceso proceso oveja
Animal
    { nombre = "Oscar"
    , tipo = "oveja"
    , peso = 50
    , edad = 16
    , estaEnfermo = False
    , visitas = []
    }
-}

--4
mejoraSustentable :: Actividad -> Animal -> Bool
mejoraSustentable actividad animal = (peso.actividad) animal > peso animal && (peso.actividad) animal < ((+3).peso) animal

mejora :: Proceso -> Animal -> Bool
mejora [] _ = True
mejora (actividad:actividades) animal = mejoraSustentable actividad animal && mejora actividades animal


--5
tresNombreFalopa :: [Animal] -> [Animal]
tresNombreFalopa lista = ((take 3).filter nombreFalopa) lista


{-
Sí sería posible, gracias a la "lazy evaluation" o evalución diferida de Haskell.
La evaluación diferida esta acctivada por defecto y gracias a ella se puede ir
generando y evaluando expresiones a medida que se accede a ellas. Por lo que en
el caso de que se pueda encontrar esos tres valores mediante el filter, sí se
podría llegar a un valor computable.
CONVERGE
-}