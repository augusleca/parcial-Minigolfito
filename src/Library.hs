module Library where
import PdePreludat

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

habilidadtest = Habilidad {
  fuerzaJugador = 20,
  precisionJugador = 10
}

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- 1) Tipos de palos

fuerzaDeHabilidad :: Habilidad -> Number
fuerzaDeHabilidad habilidad = fuerzaJugador habilidad

precisionDeHabilidad :: Habilidad -> Number
precisionDeHabilidad habilidad = precisionJugador habilidad

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {velocidad = 10, precision = ((precisionDeHabilidad habilidad) * 2), altura = 0}

madera :: Palo
madera habilidad = UnTiro {velocidad = 100, precision = ((precisionDeHabilidad habilidad) / 2), altura = 5}

hierros :: Number -> Palo
hierros n habilidad = UnTiro {velocidad = (fuerzaDeHabilidad habilidad) * n, precision = ((precisionDeHabilidad habilidad) / n), altura = max 0 (n-3)}

-- 2) Funcion golpe

golpe :: Jugador -> Palo -> Tiro
--golpe jugador palo = palo (habilidad jugador) AGREGO COMPOSICION
golpe jugador palo = (palo.habilidad) jugador

-- 3) Obstaculos

type Obstaculo = Tiro -> Tiro

inf :: Number
inf = 9999999
--tiroNulo  = UnTiro {velocidad = 0, precision = 0, altura = 0} SIMPLIFICO ESCRITURA
tiroNulo = UnTiro 0 0 0

superaObstaculo :: Tiro -> Number -> Number -> Number -> Number -> Number -> Number -> Bool
superaObstaculo tiro v1 v2 p1 p2 a1 a2
    | (between v1 v2 (velocidad tiro)) && (between p1 p2 (precision tiro)) && (between a1 a2 (altura tiro)) = True
    | otherwise = False

-- a)
tunelConRampita :: Obstaculo
tunelConRampita tiro
    | superaObstaculo tiro 1 inf 90 inf 0 0 = tiro {velocidad = velocidad tiro *2, precision = 100, altura = 0}
    | otherwise = tiroNulo

-- b)
laguna :: Obstaculo
laguna tiro
    | superaObstaculo tiro 80 inf 1 inf 1 5 = tiro {velocidad = velocidad tiro , precision = precision tiro, altura = (altura tiro) / largoLaguna}
    | otherwise = tiroNulo

largoLaguna :: Number
largoLaguna = 10 -- Por ejemplo xd

-- c)
hoyo :: Obstaculo
hoyo tiro
    | superaObstaculo tiro 5 20 95 inf 0 0 = tiro {velocidad = 0, precision = 0, altura = 0}
    | otherwise = tiroNulo

-- 4) 

-- a)
{-palosUtiles :: Jugador -> Obstaculo -> [Palo] -- TIENE QUE DEVOLVER UNA LISTA DE PALOS NO LOS PALOS APLICADOS A LOS TIROS
palosUtiles jugador obstaculo = map obstaculo (aplicarJugadorAPalos jugador)

aplicarJugadorAPalos :: Jugador -> [Tiro]
aplicarJugadorAPalos jugador = map (golpe jugador) [putter,madera,(hierros 10)] -}

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (puedeJugadorSuperarObstaculo jugador obstaculo) [putter,madera,hierros 20]

puedeJugadorSuperarObstaculo :: Jugador -> Obstaculo -> Palo -> Bool
puedeJugadorSuperarObstaculo jugador obstaculo palo = obstaculo (golpe jugador palo) /= tiroNulo

-- b)
{-cuantosObstaculosConsecutivos :: Tiro -> [Obstaculo] -> Number
cuantosObstaculosConsecutivos tiro obstaculos = length (takeWhile (atributosDistintosDeCero) (aplicarTiroAObstaculos tiro obstaculos))

atributosDistintosDeCero :: Tiro -> Bool
atributosDistintosDeCero tiro
    | superaObstaculo tiro 0 0 0 0 0 0 = False
    | otherwise = True

aplicandoFunciones valor f lista = f valor : lista
pam lista valor = foldr (aplicandoFunciones valor) [] lista

aplicarTiroAObstaculos :: Tiro -> [Obstaculo] -> [Tiro]
aplicarTiroAObstaculos tiro obstaculos = pam obstaculos tiro -}  -- NO USAR PAM

--cuantosObstaculosConsecutivos :: Tiro -> [Obstaculo] -> Number
--cuantosObstaculosConsecutivos tiro obstaculos = length (takeWhile (atributosDistintosDeCero) (aplicarTiroAObstaculos))

cuantosObstaculosConsecutivosSupero :: Tiro -> [Obstaculo] -> Number
cuantosObstaculosConsecutivosSupero tiro obstaculos = length (obstaculosConsecutivos tiro obstaculos)

obstaculosConsecutivos :: Tiro -> [Obstaculo] -> [Obstaculo]
obstaculosConsecutivos tiro obstaculos
  | atributosDistintosDeCero (aplicarTiroAObstaculo tiro (head obstaculos)) = obstaculos
  | otherwise = obstaculosConsecutivos (aplicarTiroAObstaculo tiro (head obstaculos)) (drop 1 obstaculos) 

atributosDistintosDeCero :: Tiro -> Bool
atributosDistintosDeCero tiro = tiro /= tiroNulo

aplicarTiroAObstaculo :: Tiro -> Obstaculo -> Tiro
aplicarTiroAObstaculo tiro obstaculo = obstaculo tiro 


--atributosDistintosDeCero (aplicarTiroAObstaculo (golpe bart madera) laguna) --> True








-- c)
--paloMasUtil jugador obstaculos = snd (maximum (zip [cuantosObstaculosConsecutivos (golpe jugador putter) obstaculos, cuantosObstaculosConsecutivos (golpe jugador madera) obstaculos, cuantosObstaculosConsecutivos (golpe jugador (hierros 3)) obstaculos] ["Putter","Madera","Hierros"]))

-- 5)

type Puntuaje = (Jugador,Puntos)

-- Lista ejemplo : [(bart,12),(todd,5),(rafa,10)]

padreDeCadaNiño :: Puntuaje -> (String,Puntos)
padreDeCadaNiño puntuaje = (padre (fst puntuaje), snd puntuaje)

padreGanador :: [Puntuaje] -> String
padreGanador lista = snd (foldl1 max (zip (map snd lista) (map fst (map padreDeCadaNiño lista))))

listaPadres :: [Puntuaje] -> [(Bool,String)]
listaPadres lista = zip (map (==padreGanador lista) (map fst (map padreDeCadaNiño lista))) (map fst(map padreDeCadaNiño lista))








