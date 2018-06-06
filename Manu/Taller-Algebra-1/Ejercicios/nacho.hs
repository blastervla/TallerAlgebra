
type Set a = [a]


variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones _ 0 = [[]]
variaciones xs l = insertarListaEnConjuntoLista xs (variaciones xs (l-1))

insertarListaEnConjuntoLista :: Set Integer -> Set [Integer] -> Set [Integer]
insertarListaEnConjuntoLista [] _ = []
insertarListaEnConjuntoLista (x:xs) ys = (agregarIntegerTodas x ys) ++ (insertarListaEnConjuntoLista xs ys)

agregarIntegerTodas :: Integer -> Set [Integer] -> Set [Integer]
agregarIntegerTodas _ [] = []
agregarIntegerTodas n (x:xs) = (n:x) : (agregarIntegerTodas n xs) 