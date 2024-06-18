module Library where
import PdePreludat
import Data.Foldable (Foldable(fold))

--1
type Tarea = Persona -> Persona

data Persona = Persona{
  nombre :: String,
  edad :: Number,
  energia :: Number,
  alegria :: Number,
  ansiedad :: Number,
  estres :: Number,
  tareas :: [Tarea]
}deriving (Show)

juan = Persona{
  nombre = "juan cruz",
  edad = 21,
  energia = 101,
  alegria = 75,
  ansiedad = 50,
  estres = 50,
  tareas = [codearUnProyectoNuevo, andarEnBici 2, escucharMusica]
}

cande = Persona{
  nombre = "candela belen",
  edad = 21,
  energia = 65,
  alegria = 100,
  ansiedad = 15,
  estres = 15,
  tareas = [andarEnBici 5, escucharMusica]
}

mati = Persona{
  nombre = "matias",
  edad = 21,
  energia = 0,
  alegria = 50,
  ansiedad = 50,
  estres = 50,
  tareas = [codearUnProyectoNuevo, escucharMusica]
}

asignarEstres :: Persona -> Persona
asignarEstres persona
  |(length.tareas) persona > 5 = persona{
    estres = ((*1.5).ansiedad) persona 
  }
  |otherwise = persona{
    estres = ansiedad persona
  }

asignarEnergia :: Persona -> Persona
asignarEnergia persona
  | alegria persona > ansiedad persona = persona{
    energia = min 340 (((*2).alegria) persona)
  }
  | edad persona < 40 = asignarEstres persona{
    energia = 300 - estres persona
  }
  |otherwise = persona{
    energia = alegria persona + 10
  }


--2
cuantoDueleVerLasBuenas :: [Persona] -> Bool
cuantoDueleVerLasBuenas lista = all ((>100).energia) (filter ((>=40).edad) lista)

nivelTotalDeAnsiedad :: [Persona] -> Number
nivelTotalDeAnsiedad lista = sum (map ansiedad (filter ((>=40).edad) lista))

type Criterio = Persona -> Bool

losMasCriticados :: [Persona] -> (Persona -> Bool) -> [Persona]
losMasCriticados lista criterio = take 2 (filter criterio lista)

-- > losMasCriticados [cande, juan, mati] ((>50).ansiedad)
-- > losMasCriticados [cande, juan, mati] (even.energia)  


--3
realizarTarea :: Number -> Tarea
realizarTarea valor persona = persona{
  ansiedad = (max 0 (ansiedad persona - 10 + valor)) 
}

codearUnProyectoNuevo :: Tarea
codearUnProyectoNuevo persona = realizarTarea 50 persona{
  alegria = alegria persona + 110
}

cecilia = Persona{
  nombre = "juan cruz",
  edad = 21,
  energia = 80,
  alegria = 75,
  ansiedad = 40,
  estres = 50,
  tareas = []
}

j = Persona{
  nombre = "candela belen",
  edad = 21,
  energia = 65,
  alegria = 100,
  ansiedad = 250,
  estres = 15,
  tareas = []
}

hacerTramitesEnAfip :: Number -> Tarea
hacerTramitesEnAfip cantidad persona = realizarTarea 0 persona{
  ansiedad = max 300 (((*cantidad).ansiedad) persona)
}

andarEnBici :: Number -> Tarea
andarEnBici kilometros persona = realizarTarea 0 persona{
  ansiedad = 0,
  alegria = alegria persona + 50 * kilometros
}

escucharMusica :: Tarea
escucharMusica persona = realizarTarea (-10) persona

tareitas = [codearUnProyectoNuevo, andarEnBici 5, escucharMusica]


--4 --creo que se confudieron en el enunciado y deberia ser de la ansiedad xd
energiaResultante :: Persona -> [Tarea] -> Number
energiaResultante persona lista = (energia.(foldr ($) persona)) lista 


--5 --creo que se confudieron en el enunciado y deberia ser de la ansiedad xd
intentaRealizar :: Persona -> Tarea -> Bool
intentaRealizar persona tarea = ((>100).energia.tarea) persona

hiceLoQuePude :: Persona -> [Tarea] -> Persona
hiceLoQuePude persona [] = persona
hiceLoQuePude persona (tarea:tareas)
  |intentaRealizar persona tarea = hiceLoQuePude (tarea persona) tareas
  |otherwise = persona


--6
{-
nivelTotalDeAmsiedad no se puede utilizar con una lista infinita ya que esto para no se puede garantizar que la cantidad de jovatos se finita. Es decir,
para que tener la lista filtrada por jovatos se debe recorrer toda la lista, por lo que en este caso la lazy evaluation no nos va a servir.Applicative

cuantoDueleVerLasBuenas pasa lo mismo en este caso
-}
