listar :: a -> a -> a -> [a]
listar a b c = [a, b, c]

listaDecrecienteUnoMenosCienAux :: Integer -> [Integer]
listaDecrecienteUnoMenosCienAux i   | i >= 1 = [1]
                                    | otherwise = i:listaDecrecienteUnoMenosCienAux (i + 1)

listaDecrecienteUnoMenosCien :: [Integer]
listaDecrecienteUnoMenosCien = listaDecrecienteUnoMenosCienAux (-100)

listaDecrecienteUnoMenosCienBien :: [Integer]
listaDecrecienteUnoMenosCienBien = [1,0..(-100)]

sumatoria :: [Integer] -> Integer
sumatoria lista | length lista > 0 = head lista + sumatoria (tail lista)
                | otherwise = 0

pertenece :: Integer -> [Integer] -> Bool
pertenece n lista   | length lista > 0 && head lista == n = True
                    | length lista > 0 = pertenece n (tail lista)
                    | otherwise = False

perteneceConPatternMatching :: Integer -> [Integer] -> Bool
perteneceConPatternMatching n [] = False
perteneceConPatternMatching n (a:cola) = a == n || perteneceConPatternMatching n cola

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (a:cola) = a * productoria cola

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [a] = [a + n]
sumarN n (a:cola) = (a + n):(sumarN n cola)

ultimoElemento :: [Integer] -> Integer
ultimoElemento [a] = a
ultimoElemento (b:cola) = ultimoElemento cola

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo lista = sumarN (ultimoElemento lista) lista

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (a:cola) = sumarN a (a:cola)

esPar :: Integer -> Bool
esPar n = (mod n 2) == 0

pares :: [Integer] -> [Integer]
pares [] = []
pares (a:cola)  | esPar a = a:(pares cola)
                | otherwise = pares cola

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [a] = [n * a]
multiplosDeN n (a:cola) = (n * a):(multiplosDeN n cola)

quitar :: Integer -> [Integer] -> [Integer]
quitar _ [] = []
quitar n (a:cola)   | a == n = cola
                    | otherwise = a:(quitar n cola)

hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos (a:cola) = pertenece a cola || hayRepetidos cola

eliminarRepetidos :: [Integer] -> [Integer]
eliminarRepetidos [] = []
eliminarRepetidos (a:cola) = a:eliminarRepetidos (quitar a cola)

maximo :: [Integer] -> Integer
maximo [a,b]    | a >= b = a
                | otherwise = b
maximo (a:cola) | a >= maxCola = a
                | otherwise = maxCola
                where maxCola = maximo cola

ordenar :: [Integer] -> [Integer]
ordenar [a] = [a]
ordenar lista = restoDeListaOrdenada++[mayorNum]
                where   mayorNum = maximo lista
                        restoDeListaOrdenada = (ordenar (quitar mayorNum lista))

