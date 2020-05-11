import Text.Show.Functions()

type Accion = Jugador -> Jugador

type Propiedad = (String, Int) 

data Jugador = Jugador {
    nombre :: String,
    cantidadDinero :: Int,
    tacticaJuego :: String, 
    propiedadesCompradas :: [Propiedad],
    acciones :: [Accion]
} deriving  Show

carolina = Jugador {nombre = "Carolina", cantidadDinero = 500, tacticaJuego = "Accionista", propiedadesCompradas= [], acciones = [pasarPorElBanco, pagarAAccionistas]}
manuel = Jugador {nombre = "Manuel", cantidadDinero = 500, tacticaJuego = "Oferente singular", propiedadesCompradas= [], acciones = [pasarPorElBanco, enojarse] }

sumarDinero :: Jugador -> Int -> Int
sumarDinero unJugador dinero = (cantidadDinero unJugador) + dinero

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = unJugador {cantidadDinero = sumarDinero unJugador 40, tacticaJuego  = "Comprador compulsivo"}

enojarse :: Accion
enojarse unJugador = unJugador {cantidadDinero = sumarDinero unJugador 50, acciones = gritar : (acciones unJugador)}

gritar :: Accion
gritar unJugador = unJugador {nombre =  "AHHHH" ++ (nombre unJugador)}

esAccionista :: Jugador -> Bool
esAccionista unJugador = (tacticaJuego unJugador) == "Accionista"

puedeGanarSubastas :: Jugador -> Bool
puedeGanarSubastas unJugador = (tacticaJuego unJugador) == "Oferente singular" || esAccionista unJugador

subastar :: Propiedad -> Accion
subastar unaPropiedad unJugador
    | (puedeGanarSubastas unJugador) = unJugador {cantidadDinero = sumarDinero unJugador (snd unaPropiedad)*(-1), propiedadesCompradas = unaPropiedad : (propiedadesCompradas unJugador)}
    | otherwise = unJugador

esBarata :: Propiedad -> Bool
esBarata (_, precio) = precio < 150

calcularAlquileres :: Propiedad -> Int
calcularAlquileres propiedad 
    |esBarata propiedad = 10
    |otherwise = 20

precioTotalAlquileres :: [Propiedad] -> Int
precioTotalAlquileres = sum.map calcularAlquileres

cobrarAlquileres :: Accion
cobrarAlquileres unJugador = unJugador {cantidadDinero = sumarDinero unJugador (precioTotalAlquileres (propiedadesCompradas unJugador))*(-1)}

pagarAAccionistas :: Accion
pagarAAccionistas unJugador
    | esAccionista unJugador = unJugador {cantidadDinero = sumarDinero unJugador 200} 
    | otherwise = unJugador {cantidadDinero = sumarDinero unJugador 100}
