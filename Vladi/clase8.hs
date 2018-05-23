type Set a = [a]

vacio :: Set Integer
vacio = []

agregar :: (Eq a) => a -> Set a -> Set a
agregar a conjunto  | elem a conjunto = conjunto
                    | otherwise = a:conjunto

-- Indica si el primer conjunto está incluido en el segundo
incluido :: Set Integer -> Set Integer -> Bool
incluido [] _ = True
incluido (a:resto) conjunto = (elem a conjunto) && (incluido resto conjunto)

iguales :: Set Integer -> Set Integer -> Bool
iguales c1 c2 = (incluido c1 c2) && (incluido c2 c1)

agregarATodas :: Integer -> [[Integer]] -> [[Integer]] 
agregarATodas _ [] = []
agregarATodas n (a:resto) = [(agregar n a)]++(agregarATodas n resto)

partes :: Integer -> Set (Set Integer)
partes 0 = [vacio]
partes 1 = (partes 0)++[[1]]
partes n = (partes (n - 1))++[[n],[1..n]]

partesBien :: Integer -> Set (Set Integer)
partesBien 0 = [vacio]
partesBien n = partesAnterior++(agregarATodas n partesAnterior)
                where partesAnterior = partesBien (n - 1)

concatenar :: (Eq a) => Set a -> Set a -> Set a
concatenar [] conjuntoB = conjuntoB
concatenar (a:restoA) conjuntoB = concatenar restoA (agregar a conjuntoB)

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesiano _ [] = []
productoCartesiano [] _ = []
productoCartesiano (a:restoA) (b:restoB) = agregar (a,b) (concatenar (productoCartesiano (a:restoA) restoB) (productoCartesiano restoA (b:restoB)))

listaDeElementos :: Set Integer -> Set [Integer]
listaDeElementos [] = []
listaDeElementos (x:xs) = [[x]]++(listaDeElementos xs)

agregarATodasCopyAllowed :: Integer -> [[Integer]] -> [[Integer]] 
agregarATodasCopyAllowed _ [] = []
agregarATodasCopyAllowed n (a:resto) = [n:a]++(agregarATodasCopyAllowed n resto)

agregarTodos :: [Integer] -> Set [Integer] -> [[Integer]]
agregarTodos [] conjunto = []
agregarTodos (x:xs) conjunto = (agregarATodasCopyAllowed x conjunto)++(agregarTodos xs conjunto)

variaciones :: Set Integer -> Integer -> [[Integer]]
variaciones elementos 1 = listaDeElementos elementos
variaciones elementos n = agregarTodos elementos variacionAnterior
                        where variacionAnterior = variaciones elementos (n - 1)

insertarEn :: [Integer] -> Integer -> Integer -> [Integer]
insertarEn lista n 1 = n:lista
insertarEn (x:xs) n i = x:(insertarEn (x:xs) n (i - 1))

insertarEnTodas :: [[Integer]] -> Integer -> Integer -> [[Integer]]
insertarEnTodas [] _ _ = []
insertarEnTodas (x:xs) n i = (insertarEn x n i):(insertarEnTodas (xs) n i)

insertarNVeces :: [[Integer]] -> Integer -> Integer -> [[Integer]]
insertarNVeces lista elemento n = insertarNVeces

permutaciones 1 = [[1]]
permutaciones n = (agregarTodos n permutacionAnterior)
                where permutacionAnterior = permutaciones (n - 1)

permutaciones 1 = [[1]]
permutaciones 2 = [[1,2], [2,1]]
permutaciones 3 = [[1,2,3], [2,1,3], [1,3,2], [2,3,1], [3,1,2], [3,2,1]]

