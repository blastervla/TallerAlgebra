---- Bloque 1 Ejercicios ----
--- Ej. a) ---
--- Definir la funci´on listar :: a -> a -> a -> [a] que toma 3 elementos y los convierte
--- en una lista.

listar :: a -> a -> a -> [a]
listar a b c = [a, b, c]

--- Ej. b) ---
--- Escribir una expresi´on que denote la lista estrictamente decreciente de enteros que comienza
--- con el n´umero 1 y termina con el n´umero -100.

-- Forma complicada
crearListaDescendiente :: Integer -> Integer -> [Integer] -> [Integer]
crearListaDescendiente n h l
            | n == h = l ++ [n]
            | n > h = crearListaDescendiente (n-1) h (l ++ [n])

listaDecreciente :: [Integer]
listaDecreciente = crearListaDescendiente 1 (-100) []

-- Mas facil
listaDescendiente = [1,0..(-100)]

---- Bloque 2 Ejercicios ----
--- Ej. a) ---
--- Sumatoria de los elementos de una lista
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x: xs ) = sumatoria xs + x

--- Ej. b) ---
--- Si un elemento pertenece a una lista
-- pertenece :: Integer -> [Integer] -> Bool
-- que indica si un elemento aparece en la lista. Por ejemplo:
-- pertenece 9 [] False
-- pertenece 9 [1,2,3] False
-- pertenece 9 [1,2,9,9,-1,0] True

pertenece :: Integer -> [Integer] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | x == n = True
                   | otherwise = pertenece n (xs)

---- Bloque 3 Ejercicios ----
-- Resolver primero sin y despues con pattern matching sobre listas

--- Ej. a) ---
-- productoria :: [Integer] -> Integer que devuelve la productoria de los elementos.
productoria :: [Integer] -> Integer
productoria [] = 1 --esta definido asi bancatela
productoria (x:xs) = x * productoria xs

--- Ej. b) ---
-- sumarN :: Integer -> [Integer] -> [Integer] que dado un n´umero N y una lista xs,
-- suma N a cada elemento de xs.
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (x:xs) = (x + n : sumarN n xs)

--- Ej. c) ---
-- sumarElUltimo :: [Integer] -> [Integer] que dada una lista no vac´ıa xs, suma el
-- ultimo elemento a cada elemento de xs. Ejemplo sumarElUltimo [1,2,3] [4,5,6]
lastMio :: [Integer] -> Integer
lastMio (x:[]) = x
lastMio (x:xs) = last xs

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo xs = sumarN (lastMio xs) xs

--- Ej. d) ---
-- sumarElPrimero :: [Integer] -> [Integer] que dada una lista no vacía xs, suma el
-- primer elemento a cada elemento de xs. Ejemplo sumarElPrimero [1,2,3] [2,3,4]
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs)

--- Ej. e) ---
-- pares :: [Integer] -> [Integer] que devuelve una lista con los elementos pares de la
-- lista original. Ejemplo pares [1,2,3,8] [2,8]
esPar :: Integer -> Bool
esPar n = n `mod` 2 == 0

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | esPar x = x : pares xs
             | otherwise = pares xs

--- Ej. f) ---
-- multiplosDeN :: Integer -> [Integer] -> [Integer] que dado un n´umero N y una
-- lista xs, devuelve una lista con los elementos multiplos N de xs.
esMultDeN :: Integer -> Integer -> Bool
esMultDeN k n = k `mod` n == 0

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | esMultDeN x n = x : multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

--- Ej. g) ---
-- quitar :: Integer -> [Integer] -> [Integer] que elimina la primera aparición del
-- elemento en la lista (de haberla).
quitar :: Integer -> [Integer] -> [Integer]
quitar _ [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = (x : quitar n xs)

--- Ej. h) ---
-- hayRepetidos :: [Integer] -> Bool que indica si una lista tiene elementos repetidos.
hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | x `pertenece` xs = True
                    | otherwise = hayRepetidos xs

--- Ej. i) ---
-- eliminarRepetidos :: [Integer] -> [Integer] que deja en la lista una ´unica aparici´on
-- de cada elemento, eliminando las repeticiones adicionales.
eliminarRepetidos :: [Integer] -> [Integer]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | x `pertenece` xs = eliminarRepetidos (x : (quitar x xs))
                         | otherwise = x : eliminarRepetidos xs
--- Ej. j) ---
-- maximo :: [Integer] -> Integer que calcula el m´aximo elemento de una lista no vacía.
mayorATodos :: Integer -> [Integer] -> Bool
mayorATodos n [] = True
mayorATodos n (x:xs) | n < x = False
                     | otherwise = mayorATodos n xs

maximo :: [Integer] -> Integer
maximo (x:[]) = x
maximo (x:xs) | mayorATodos x xs = x
              | otherwise = maximo xs

--- Ej. k) ---
-- ordenar :: [Integer] -> [Integer] que ordena los elementos de forma creciente.
menorATodos :: Integer -> [Integer] -> Bool
menorATodos n [] = True
menorATodos n (x:xs) | n > x = False
                     | otherwise = menorATodos n xs

minimo :: [Integer] -> Integer
minimo (x:[]) = x
minimo (x:xs) | menorATodos x xs = x
              | otherwise = minimo xs

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar xs = m : ordenar (quitar m xs)
                where m = minimo xs