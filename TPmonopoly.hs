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

cambiarNombre :: String -> Accion
cambiarNombre prefijo unJugador= unJugador {nombre = prefijo ++ (nombre unJugador)}

cambiarCantidadDinero :: Int -> Accion
cambiarCantidadDinero monto unJugador= unJugador {cantidadDinero = (cantidadDinero unJugador) + monto}

cambiarTacticaJuego :: String -> Accion
cambiarTacticaJuego unaTactica unJugador= unJugador {tacticaJuego = unaTactica}

cambiarPropiedadesCompradas :: Propiedad -> Accion
cambiarPropiedadesCompradas unaPropiedad unJugador= unJugador {propiedadesCompradas = unaPropiedad : (propiedadesCompradas unJugador)}

cambiarAcciones :: Accion -> Accion
cambiarAcciones unaAccion unJugador= unJugador {acciones = unaAccion : (acciones unJugador)}

precioPropiedad :: Propiedad -> Int
precioPropiedad unaPropiedad = snd unaPropiedad

pasarPorElBanco :: Accion
pasarPorElBanco= cambiarCantidadDinero 40 .cambiarTacticaJuego "Comprador compulsivo"

enojarse :: Accion
enojarse= cambiarCantidadDinero 50 .cambiarAcciones enojarse

gritar :: Accion
gritar= cambiarNombre "AHHHH"

esAccionista :: Jugador -> Bool
esAccionista unJugador = (tacticaJuego unJugador) == "Accionista"

puedeGanarSubastas :: Jugador -> Bool
puedeGanarSubastas unJugador = (tacticaJuego unJugador) == "Oferente singular" || esAccionista unJugador

comprarPropiedad :: Propiedad -> Accion
comprarPropiedad unaPropiedad unJugador = cambiarCantidadDinero (- precioPropiedad unaPropiedad) (cambiarPropiedadesCompradas unaPropiedad unJugador)

subastar :: Propiedad -> Accion
subastar unaPropiedad unJugador
    | puedeGanarSubastas unJugador= comprarPropiedad unaPropiedad unJugador
    | otherwise = unJugador 

esBarata :: Propiedad -> Bool
esBarata unaPropiedad = (precioPropiedad unaPropiedad) < 150

calcularAlquileres :: Propiedad -> Int
calcularAlquileres unaPropiedad 
    |esBarata unaPropiedad = 10
    |otherwise = 20

precioTotalAlquileres :: [Propiedad] -> Int
precioTotalAlquileres = sum.map calcularAlquileres

cobrarAlquileres :: Accion
cobrarAlquileres unJugador= cambiarCantidadDinero (- precioTotalAlquileres (propiedadesCompradas unJugador)) unJugador

pagarAAccionistas :: Accion
pagarAAccionistas unJugador
    | esAccionista unJugador= cambiarCantidadDinero 200 unJugador  
    | otherwise = cambiarCantidadDinero (-100) unJugador

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unJugador 
    | (cantidadDinero unJugador) < (precioPropiedad unaPropiedad) = hacerBerrinchePor unaPropiedad (cambiarAcciones gritar (cambiarCantidadDinero 10  unJugador))
    | otherwise = comprarPropiedad unaPropiedad unJugador  

ultimaRonda :: Jugador -> Accion
ultimaRonda unJugador = foldl1 (.) (acciones unJugador)

juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal caro manu
    | (cantidadDinero ((ultimaRonda caro) caro)) > (cantidadDinero ((ultimaRonda manu) manu)) = caro
    | otherwise = manu