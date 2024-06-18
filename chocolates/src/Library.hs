module Library where
import PdePreludat
import Data.Foldable (Foldable(fold))
--CHOCOLATES

data Ingrediente = Ingrediente{
  nombreIgrediente :: String,
  calorias :: Number
}deriving (Show)

data Chocolate = Chocolate{
  ingredientes :: [Ingrediente],
  nombred :: String,
  precio :: Number,
  tipo :: String,
  gramaje :: Number,
  esDiabetico :: Bool
}deriving (Show)

normal = Chocolate{
  ingredientes = [Ingrediente "1" 10, Ingrediente "2" 90, Ingrediente "3" 200],
  nombred = "blanco",
  precio = 23,
  tipo = "asdad",
  gramaje = 231,
  esDiabetico = False
}

raro = Chocolate{
  ingredientes = [Ingrediente "9" 100, Ingrediente "5" 100, Ingrediente "7" 200],
  nombred = "raro",
  precio = 23,
  tipo = "asdad",
  gramaje = 231,
  esDiabetico = False
}

rico = Chocolate{
  ingredientes = [Ingrediente "1" 50, Ingrediente "2" 100],
  nombred = "rico",
  precio = 23,
  tipo = "asdad",
  gramaje = 231,
  esDiabetico = False
}

feo = Chocolate{
  ingredientes = [Ingrediente "9" 50],
  nombred = "feo",
  precio = 23,
  tipo = "asdad",
  gramaje = 231,
  esDiabetico = False
}

--1
precioPremiun :: Chocolate -> Number
precioPremiun chocolate
  |esDiabetico chocolate = 8 * gramaje chocolate
  |otherwise = 5 * gramaje chocolate

asignarPrecio :: Chocolate -> Chocolate
asignarPrecio chocolate
  | ((=="amargo" ).tipo) chocolate = chocolate{
    precio = gramaje chocolate* precioPremiun chocolate
  }
  | (length.ingredientes) chocolate > 4 = chocolate{
    precio = ((*8).length.ingredientes) chocolate
  }
  |otherwise = chocolate{
    precio = ((*1.5).gramaje) chocolate
  }


--2
esBombonAsesino :: Chocolate -> Bool
esBombonAsesino chocolate = (any ((>200).calorias).ingredientes) chocolate

totalCalorias  :: Chocolate -> Number
totalCalorias chocolate = (sum.(map calorias.ingredientes)) chocolate
--totalCalorias  chocolate = sum (map calorias (ingredientes chocolate))

aptoParaNinios :: [Chocolate] -> [Chocolate]
aptoParaNinios lista = take 3 (filter (not.esBombonAsesino) lista)


--3
type Proceso = Chocolate -> Chocolate

agregarIngrediente :: String -> Number -> Proceso
agregarIngrediente nuevo calorias chocolate = chocolate{
  ingredientes = ingredientes chocolate ++ [Ingrediente nuevo calorias]
}

frutalizado :: Number -> String -> Proceso
frutalizado cantidad fruta chocolate = chocolate{
  ingredientes = ingredientes chocolate ++ [Ingrediente fruta (2*cantidad)]
}

dulceDeLeche :: Proceso
dulceDeLeche chocolate = agregarIngrediente "dulce de leche" 220 chocolate{
  --ingredientes = ingredientes chocolate ++ [Ingrediente "dulce de leche" 220],
  nombred = nombred chocolate ++"tentacion"
}

celiaCrucera :: Number -> Proceso
celiaCrucera porcentaje chocolate = chocolate{
  ingredientes = ingredientes chocolate ++ [Ingrediente "azucar" porcentaje]
}

embriagadora :: Number -> Proceso
embriagadora grado chocolate = celiaCrucera 20 chocolate{
  ingredientes = ingredientes chocolate ++ [Ingrediente "licor" (min 30 grado)]
}


--4
receta = [agregarIngrediente "naranja" 20, dulceDeLeche, embriagadora 32]


--5
aplicarReceta :: [Proceso] -> Proceso
aplicarReceta procesos chocolate = foldr ($) chocolate procesos


--6
data Persona = Persona{
  saturacion :: Number,
  caloriasPersona :: Number,
  criterio :: String
}deriving (Show)

juan = Persona{
  saturacion = 851,
  caloriasPersona = 0,
  criterio = "mani"
}

cambiarCalorias :: Number -> Persona -> Persona
cambiarCalorias calorias persona = persona{
  caloriasPersona = calorias
}

hastaAcaLlegue :: Persona -> [Chocolate] -> [Chocolate]
hastaAcaLlegue _ [] = [] 
hastaAcaLlegue persona (chocolate:chocolates)
  |(caloriasPersona persona + totalCalorias chocolate) < saturacion persona && all ((/= criterio persona).nombreIgrediente) (ingredientes chocolate) = chocolate : hastaAcaLlegue (cambiarCalorias (caloriasPersona persona + totalCalorias chocolate) persona) chocolates
  |otherwise = hastaAcaLlegue persona chocolates

--7
{-
evaluacion diferida: el primero se puede porque solo se necesitan 3, lo que es posible dada la lazy evalutation,
lo segundo no se puede porque para dar la sumatoria de calorias hay que recorrer toda la lista infinita, imposible
-}