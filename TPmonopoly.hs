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

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = Jugador {cantidadDinero = (cantidadDinero unJugador) + 40, tacticaJuego  = "Comprador compulsivo"}

enojarse :: Accion
enojarse unJugador = Jugador {cantidadDinero = (cantidadDinero unJugador) + 50, acciones = (acciones unJugador) ++ [gritar]}

gritar :: Accion
gritar unJugador = Jugador {nombre =  "AHHHH" ++ (nombre unJugador)}

esAccionista :: Jugador -> Bool
esAccionista unJugador = (tacticaJuego unJugador) == "Accionista"

puedeGanarSubastas :: Jugador -> Bool
puedeGanarSubastas unJugador = (tacticaJuego unJugador) == "Oferente singular" || esAccionista unJugador

subastar :: Propiedad -> Accion
subastar unaPropiedad unJugador| (puedeGanarSubastas unJugador) = Jugador {cantidadDinero = (cantidadDinero unJugador) - (snd unaPropiedad), propiedadesCompradas = (propiedadesCompradas unJugador) ++ [unaPropiedad]} | otherwise = unJugador

esBarata :: Propiedad -> Bool
esBarata (_, precio) = precio < 150

esCara :: Propiedad -> Bool
esCara (_, precio) = precio >= 150

precioTotalAlquileres :: [Propiedad] -> Int
precioTotalAlquileres prop = (length (filter esBarata prop))*10 + (length (filter esCara prop))*20

cobrarAlquileres :: Accion
cobrarAlquileres unJugador = Jugador {cantidadDinero = (cantidadDinero unJugador) - (precioTotalAlquileres (propiedadesCompradas unJugador))}

pagarAAccionistas :: Accion
pagarAAccionistas unJugador | esAccionista unJugador = Jugador {cantidadDinero = (cantidadDinero unJugador) + 200} | otherwise = Jugador {cantidadDinero = (cantidadDinero unJugador) - 100}
