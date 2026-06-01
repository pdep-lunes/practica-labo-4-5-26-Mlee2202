module Parcial where
import Text.Show.Functions()

data Perro = UnPerro {
    raza :: String,
    juguetesFavoritos :: [String],
    tiempoEnLaGuarderia :: Int,
    energia :: Int
} deriving (Show, Eq)

data Guarderia = UnaGuarderia {
    nombre :: String,
    rutina :: [Actividad]
} deriving (Show)

type Ejercicio = Perro -> Perro
type Actividad = (Ejercicio, Int)

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = UnaGuarderia {
    nombre = "GuarderiaPdePerritos",
    rutina = [
    (jugar, 30),
    (ladrar 18, 20),
    (regalar "pelota", 0),
    (diaDeSpa, 120),
    (diaDeCampo, 720)
    ]
} 

zara :: Perro
zara = UnPerro {
    raza = "Dalmata",
    juguetesFavoritos = ["Pelota","Mantita"],
    tiempoEnLaGuarderia = 90,
    energia = 80
}

luis :: Perro
luis = UnPerro {
    raza = "Pomeranian",
    juguetesFavoritos = ["Peluche"],
    tiempoEnLaGuarderia = 1000,
    energia = 80
}

------------------ Ejercicios --------------------------

jugar :: Ejercicio
jugar = modificarEnergia (subtract 10)

ladrar :: Int -> Ejercicio
ladrar unosLadridos = modificarEnergia (+ (div unosLadridos 2))

regalar :: String -> Ejercicio
regalar unJuguete = agregarJuguete unJuguete

diaDeSpa :: Ejercicio
diaDeSpa unPerro
    |esRazaExtravagante unPerro = efectosDiaDeSpa unPerro
    |tiempoEnLaGuarderia unPerro > 50 = efectosDiaDeSpa unPerro
    |otherwise = unPerro

diaDeCampo :: Ejercicio
diaDeCampo unPerro = jugar.eliminarJuguete $ unPerro

------------------ Funciones Auxiliares --------------------------

modificarEnergia :: (Int -> Int) -> Perro -> Perro
modificarEnergia unaFuncion unPerro = unPerro{energia = (max 0).unaFuncion.energia $ unPerro }

modificarJuguete :: ([String] -> [String]) -> Perro -> Perro
modificarJuguete unaOperacion unPerro = unPerro {juguetesFavoritos = unaOperacion.juguetesFavoritos $ unPerro}

eliminarJuguete :: Perro -> Perro
eliminarJuguete unPerro = modificarJuguete (drop 1) unPerro

agregarJuguete :: String -> Perro -> Perro
agregarJuguete unJuguete unPerro = modificarJuguete (unJuguete :) unPerro

efectosDiaDeSpa :: Perro -> Perro
efectosDiaDeSpa unPerro = (modificarEnergia (const 100)).(agregarJuguete "Peine De Goma") $ unPerro

esRazaExtravagante :: Perro -> Bool
esRazaExtravagante (UnPerro "Dalmata" _ _ _) = True
esRazaExtravagante (UnPerro "Pomerania" _ _ _) = True
esRazaExtravagante _ = False

------------------ Parte B ------------------------------------------

puedeEstar :: Guarderia -> Perro -> Bool
puedeEstar unaGuarderia unPerro = tiempoEnLaGuarderia unPerro > tiempoTotalRutina unaGuarderia 

tiempoTotalRutina :: Guarderia -> Int
tiempoTotalRutina = sum.map obtenerTiempo.rutina

obtenerTiempo :: Actividad -> Int
obtenerTiempo (_,tiempo) = tiempo

esPerroResponsable :: Perro -> Bool
esPerroResponsable = (> 3).cantidadDeJuguetes.diaDeCampo

cantidadDeJuguetes :: Perro -> Int
cantidadDeJuguetes unPerro = length.juguetesFavoritos $ unPerro

realizarRutina :: Guarderia -> Perro -> Perro
realizarRutina unaGuarderia unPerro
    |puedeEstar unaGuarderia unPerro = foldl realizarActividad unPerro (rutina unaGuarderia)
    |otherwise = unPerro

realizarActividad ::  Perro -> Actividad -> Perro
realizarActividad unPerro (ejercicio,_) = ejercicio unPerro

reporteDeCansados :: Guarderia -> [Perro] -> [Bool]
reporteDeCansados unaGuarderia = map (estaCansado.realizarRutina unaGuarderia)

estaCansado :: Perro -> Bool
estaCansado = (< 5).energia