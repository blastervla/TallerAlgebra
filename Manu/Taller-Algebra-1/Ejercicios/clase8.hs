-- Defino el tipo Set
type Set a = [a]

-- ========================
-- ||      Bloque 1      ||
-- ========================
-- Defino al conjunto vacio
vacio :: Set Integer
vacio = []

-- Defino la funncion agregar
-- Dado un número n y un conjunto S nos devuelva un conjunto
agregar :: Integer -> Set Integer -> Set Integer
agregar n s | n `elem` s = s
            | otherwise = (n : s)

-- ========================
-- ||      Bloque 2      ||
-- ========================

--- Ej. 1 ---
-- incluido :: Set Integer -> Set Integer -> Bool que determina si el primer conjunto
-- está incluido en el segundo

incluido :: Set Integer -> Set Integer -> Bool
incluido [] _ = True
incluido (x:[]) s = (x `elem` s)
incluido (x:xs) s | not (x `elem` s) = False
                  | otherwise = incluido xs s

--- Ej. 2 ---
-- iguales :: Set Integer -> Set Integer -> Bool que determina si dos conjuntos son
-- iguales. (nota: tener en cuenta que a los conjuntos no les interesa el orden)

iguales :: Set Integer -> Set Integer -> Bool
iguales s t = (s `incluido` t) && (t `incluido` s)

-- ========================
-- ||      Bloque 3      ||
-- ========================

--- Entre todos ---
-- Implementar entre todos la función
-- agregarATodas :: Integer -> Set [Integer] -> Set [Integer] que dado un número n y
-- una lista de listas lls agrega a n detrás de cada lista de lls
-- Ejemplo: 2 -> [[], [1], [2]] -> [[2], [2,1], [2,2]]
agregarATodas :: Integer -> Set [Integer] -> Set [Integer]
agregarATodas _ [] = []
agregarATodas n (x:xs) = (n : x) : agregarATodas n xs

--- Ej. 1 ---
-- Implementar una función
-- partes :: Integer -> Set (Set Integer) que genere todos los subconjuntos del
-- conjunto {1, 2, 3, . . . , n}.
-- Ejemplo> partes 2
-- [[], [1], [2], [1, 2]]

partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = partes (n - 1) ++ agregarATodas n (partes (n-1))

-- ========================
-- ||      Bloque 4      ||
-- ========================

-- Implementar una función
-- productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
-- que dados dos conjuntos genere todos los pares posibles (como pares de dos elementos)
-- tomando el primer elemento del primer conjunto y el segundo elemento del segundo
-- conjunto.
-- Ejemplo> productoCartesiano [1, 2, 3] [3, 4]
-- [(1, 3), (2, 3), (3, 3), (1, 4), (2, 4), (3, 4)]

combinacionesDe :: Integer -> Set Integer -> Set (Integer, Integer)
combinacionesDe n [] = []
combinacionesDe n (x:xs) = (n, x) : combinacionesDe n xs

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesiano [] _ = []
productoCartesiano (x:xs) ys = combinacionesDe x ys ++ productoCartesiano xs ys


--- Ej. 1 ---
-- Implementar una función
-- variaciones :: Set Integer -> Integer -> [[Integer]] que dado un conjunto c y
-- una longitud l genere todas las posibles listas de longitud l a partir de elementos de c.
-- Ejemplo> variaciones [4, 7] 3
-- [[4, 4, 4], [4, 4, 7], [4, 7, 4], [4, 7, 7], [7, 4, 4], [7, 4, 7], [7, 7, 4], [7, 7, 7]]
-- variaciones :: (Integer, Integer) -> Integer -> [[Integer]]  
-- variaciones _ 0 = [[]]
-- variaciones (x,y) n = agregarATodas x (variaciones (x,y) (n-1)) ++ agregarATodas y (variaciones (x,y) (n-1))

variacionesManteniendoInicial :: Set Integer -> Set Integer -> Integer -> [[Integer]]  
variacionesManteniendoInicial _ _ 0 = [[]]
variacionesManteniendoInicial (x:[]) (y:ys) n = agregarATodas x variacionesAnteriores
    where variacionesAnteriores = variacionesManteniendoInicial (y:ys) (y:ys) (n-1)

variacionesManteniendoInicial (x:xs) (y:ys) n = (agregarATodas x variacionesAnteriores) ++ variacionesDelResto
    where variacionesDelResto = variacionesManteniendoInicial xs (y:ys) n
          variacionesAnteriores = variacionesManteniendoInicial (y:ys) (y:ys) (n-1)    
          
          
variaciones :: Set Integer -> Integer -> [[Integer]]
variaciones (x:xs) n = variacionesManteniendoInicial (x:xs) (x:xs) n