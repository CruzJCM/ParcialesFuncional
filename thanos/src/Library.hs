module Library where
import PdePreludat
import Data.Foldable (Foldable(fold))
import GHC.Arr (listArray)
--THANOS

--1

data Guantelete = Guantelete{
  material :: String,
  gemas :: [Poder]
}deriving (Show)

data Personaje = Personaje{
  edad :: Number,
  energia :: Number,
  habilidades :: [String],
  nombre :: String,
  planeta :: String
}deriving (Show)


spiderman = Personaje{
  edad = 26,
  energia = 200,
  habilidades = ["trepar", "disparar", "fuerza", "sentido"],
  nombre = "Peter",
  planeta = "Tierra"
}

type Universo = [Personaje]

guanteletePeligroso :: Guantelete -> Bool
guanteletePeligroso guantelete = ((==6).length.gemas) guantelete && ((=="uru").material) guantelete

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo
  |guanteletePeligroso guantelete = take (((/2).length) universo) universo
  |otherwise = universo

--2
aptoPendex :: Universo -> Bool
aptoPendex universo = any ((<45).edad) universo

filtroHabilidad :: Universo -> Universo
filtroHabilidad universo = filter ((>1).length.habilidades) universo

energiaTotal :: Universo -> Number
energiaTotal universo = (sum.(map energia).filtroHabilidad) universo
--energiaTotal universo = sum (map energia (filtroHabilidad universo))


--3
type Poder = Personaje -> Personaje

modificarEnergia :: Number -> Poder
modificarEnergia valor personaje = personaje{
  energia = energia personaje + valor
}

mente :: Number -> Poder
mente valor personaje = personaje{
  energia = energia personaje - valor
}

alma :: String -> Poder
alma habilidad personaje
  |(any (==habilidad).habilidades) personaje = personaje{
    habilidades = (filter (/=habilidad).habilidades) personaje,
    energia = energia personaje - 10
  }
  |otherwise = personaje

espacio :: String -> Poder
espacio nuevoPlaneta personaje = modificarEnergia (-20) personaje{
  planeta = nuevoPlaneta
}

poder :: Poder
poder personaje
  |((<=2).length.habilidades) personaje = personaje{
    habilidades = [],
    energia = 0
  }
  |otherwise = personaje{
    energia = 0
  }

tiempo :: Poder
tiempo personaje = modificarEnergia (-50) personaje{
  edad = min 18 (edad personaje/2)
}

realidad :: Poder -> Poder
realidad gema personaje = (gema.gema) personaje


--4
--goma = [tiempo, alma "usar Mjolnir", realidad(alma "programación en Haskell")]
goma = Guantelete{
  material = "goma",
  gemas = [tiempo, alma "usar Mjolnir", realidad (alma "programación en Haskell")]
}

--5
utilizar :: [Poder] -> Poder
utilizar lista personaje = foldr ($) personaje lista

--6
{-SOLUCION CON ORDEN SUPERIOR (NO DAR PELOTA, NO ES LO QUE SE PIDE)

data Gema = Gema{
  nombreGema :: String,
  perdidaEnergia :: Number
}deriving (Show)

gemaMasPoderosa :: Guantelete -> String
gemaMasPoderosa (Guantelete _ gemas _) =  (nombreGema.head) (filter ((==(maximum (map perdidaEnergia gemas))).perdidaEnergia) gemas)
-}

maximaPerdida :: Poder -> [Poder] -> Personaje -> [Poder]
maximaPerdida gema (gemanext:gemas) personaje
  |(energia.gema) personaje < (energia.(head gemas)) personaje = gemas
  |otherwise = (gema:gemas)

gemaMasPoderosa :: Guantelete -> Personaje -> Poder
gemaMasPoderosa (Guantelete material (gema:gemas)) personaje = gemaMasPoderosa (Guantelete material (maximaPerdida gema gemas personaje)) personaje

--7
{-
usoLasTresPrimerasGemas guanteleteDeLocos punisher: esta si se puede usar por lazy evaluation

gemaMasPoderosa punisher guanteleteDeLocos: esta no se puede porque para sacar conclusion debe
recorrer toda la lista
-}