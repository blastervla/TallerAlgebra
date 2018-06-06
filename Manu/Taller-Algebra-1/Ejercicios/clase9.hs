-- ========================
-- ||      Bloque 1      ||
-- ========================

-- Filtrar por múltiplos de 7
-- Definir una función que dada una lista l de enteros, devuelva una lista l'
-- con los valores de l que son múltiplo de 7.

esMultDe7 :: Integer -> Bool
esMultDe7 n = n `mod` 7 == 0

multiplosDe7 :: [Integer] -> [Integer]
multiplosDe7 [] = []
multiplosDe7 (x:xs) | esMultDe7 x = x : (multiplosDe7 xs)
                    | otherwise = multiplosDe7 xs

-- ========================
-- ||      Bloque 2      ||
-- ========================

--- Insertar un elemento en una lista ---
-- Implementar una función
-- insertarEn :: [Integer] -> Integer -> Integer -> [Integer] que dados una lista l,
-- un número n y una posición i (contando desde 1) devuelva una lista en donde se insertó n
-- en la posición i de l y los elementos siguientes corridos en una posición.
-- Ejemplo> insertarEn [1, 2, 3, 4, 5] 6 2
-- [1, 6, 2, 3, 4, 5]

-- Solución fea
insertarEnFeo :: [Integer] -> Integer -> Integer -> [Integer]
insertarEnFeo ls n i = insertarEnGuardandoLongitud ls n i (length ls)

insertarEnGuardandoLongitud :: [Integer] -> Integer -> Integer -> Int -> [Integer]
insertarEnGuardandoLongitud (x:xs) n i t 
    | i > (fromIntegral t) = (x:xs) ++ [n]
    | i == indiceActual = n : (x:xs)
    | otherwise = x : (insertarEnGuardandoLongitud (xs) n i t)
    where indiceActual = fromIntegral(t - length xs)

-- Solución linda
insertarEn :: [Integer] -> Integer -> Integer -> [Integer]
insertarEn xs n 1 = n : xs
insertarEn (x:xs) n i = x : (insertarEn xs n (i - 1))

--- Permutaciones (DIFÍCIL!) ---
-- Implementar una función
-- permutaciones :: Integer -> [[Integer]]
-- que genere todas las posibles permutaciones de los números del 1 al n.
-- Ejemplo> permutaciones 3
-- [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]

permutaciones :: Integer -> [[Integer]]
permutaciones 0 = [[]]
permutaciones n = insertarEnTodosLosIndices (permutaciones (n - 1)) n

-- Por cada permutación anterior, inserto n a todas, en todos los posibles indices

-- Inserta n a todas las listas en todos los indices posibles
-- Ejemplo> insertarEnTodosLosIndices [[1,2] [2,1]]
-- [[1,2,3],[2,1,3],[1,3,2],[2,3,1],[3,1,2],[3,2,1]]
insertarEnTodosLosIndices :: [[Integer]] -> Integer -> [[Integer]]
insertarEnTodosLosIndices [[]] n = [[n]]
insertarEnTodosLosIndices (x:xs) n = insertarEnTodosLosIndicesCE (x:xs) n longitudPermutaciones
        where longitudPermutaciones = fromIntegral (length x) + 1

-- CE: Comenzando en
insertarEnTodosLosIndicesCE :: [[Integer]] -> Integer -> Integer -> [[Integer]]
insertarEnTodosLosIndicesCE lls n 1 = insertarEnTodas lls n 1
insertarEnTodosLosIndicesCE lls n i = insertarEnTodas lls n i ++ (insertarEnTodosLosIndicesCE lls n (i - 1))

-- insertarEnTodas inserta n a todas las listas en la posición i
-- Ejemplo> insertarEnTodas [[2,1] [1,2]] 3 2
-- [[2,3,1], [1,3,2]]
insertarEnTodas :: [[Integer]] -> Integer -> Integer -> [[Integer]]
insertarEnTodas [] _ _ = []
insertarEnTodas (x:xs) n i = insertarEn x n i : insertarEnTodas xs n i

-- ========================
-- ||      Bloque 3      ||
-- ========================

--- Ej. 1 ---
-- Todas las formas de ubicar n bolitas numeradas en k cajas.
-- Todas las formas de agregar los elementos de una lista de n numeros a una lista de k listas

insertarEnTodasLasListas :: [[Integer]] -> Integer -> [[Integer]]
insertarEnTodasLasListas (x:xs) n = insertarEnTodas (x:xs) n (fromIntegral (length x))
ordenamientosBolitasEnCajas :: Integer -> [[Integer]] -> [[Integer]]
ordenamientosBolitasEnCajas 0 c = c
ordenamientosBolitasEnCajas n c = insertarEnTodasLasListas (ordenamientosBolitasEnCajas (n - 1) c) n

