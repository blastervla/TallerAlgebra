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
insertarEn (x:xs) n i = x:(insertarEn (xs) n (i - 1))

insertarEnTodas :: [[Integer]] -> Integer -> Integer -> [[Integer]]
insertarEnTodas [] _ _ = []
insertarEnTodas (x:xs) n i = (insertarEn x n i):(insertarEnTodas xs n i)

insertarNVeces :: [[Integer]] -> Integer -> Integer -> [[Integer]]
insertarNVeces _ _ 0 = []
insertarNVeces lista elemento n = (insertarEnTodas lista elemento n)++(insertarNVeces lista elemento (n-1))

-- permutaciones 1 = [[1]]
-- permutaciones 2 = [[1,2], [2,1]]
-- permutaciones 3 = [[1,2,3], [2,1,3], [1,3,2], [2,3,1], [3,1,2], [3,2,1]]
-- permutaciones 4 = [[1,2,3,4], [2,1,3,4], [1,3,2,4], [2,3,1,4], [3,1,2,4], [3,2,1,4]]

-- agrego n veces n en la primera posición de cada elemento de mi permutación anterior
-- agrego n veces n en la segunda posición de cada elmento de mi permutación anterior
-- ... (and so on)
permutaciones :: Integer -> [[Integer]]
permutaciones 1 = [[1]]
permutaciones n = insertarNVeces permutacionAnterior n n
                where permutacionAnterior = permutaciones (n - 1)

crearNListasVacias :: Integer -> [[Integer]]
crearNListasVacias 0 = []
crearNListasVacias n = []:(crearNListasVacias (n-1))

insertarHastaIndiceConcatenando :: [[Integer]] -> Integer -> Integer -> [[[Integer]]]
insertarHastaIndiceConcatenando [x] elemento i = [[insertarEn x elemento i]]
insertarHastaIndiceConcatenando (x:xs) elemento i = [(insertarEn x elemento i)]:(insertarHastaIndiceConcatenando xs elemento (i-1))

nBolitasEnKCajas :: Integer -> Integer -> [[[Integer]]]
nBolitasEnKCajas 0 k = [crearNListasVacias k]
nBolitasEnKCajas n k = insertarHastaIndiceConcatenando elementoAnterior n n
                    where elementoAnterior = head (nBolitasEnKCajas (n-1) k)

    -- insertar en primera coordenada ++ insertar en segunda ++...hasta insertar en n

insertarEnTodasNVeces :: [[[Integer]]] -> Integer -> Integer -> [[[Integer]]]
insertarEnTodasNVeces [] _ _ = [[]]
insertarEnTodasNVeces (x:xs) elemento i = (insertarEnTodas x elemento i):(insertarEnTodasNVeces xs elemento i)



-- n = 0, k = 2 --> [ [ [],[] ] ]

-- Para n = 1, tomo esta lista y le agrego 1 en el primer elemento (head) luego le agrego 1 en el segundo

-- Para n = 2, tomo n=1 y tomo todos los elementos de la lista y les agrego el 2 en el primer indice. 
-- Después hago lo mismo pero en el 2do indice

-- n = 1, k = 2 --> [ [ [1],[] ],[ [],[1] ] ] agrego [1..n]
-- n = 2, k = 2 --> [ [ [1,2],[] ],[ [],[1,2] ], [ [1],[2] ],[ [2],[1] ] ]
-- n = 3, k = 2 -> [[[1,2,3][]],[[][1,2,3]], [[[1][2,3]],[[2,3][1]]], [[1,2][3]],[[3][1,2]], [[[1,3][2]],[[2][1,3]]]]

