--- Clase 6 ---

--- Ejercicios ---

--- Ej. (a) ---
-- yLogico :: Bool -> Bool -> Bool, la conjunción lógica.
yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _ = False


--- Ej. (b) ---
-- oLogico :: Bool -> Bool -> Bool, la disyunción lógica.
oLogico :: Bool -> Bool -> Bool
oLogico False False = False
oLogico _ _ = True


--- Ej. (c) ---
-- implica :: Bool -> Bool -> Bool, la implicación lógica.
implica :: Bool -> Bool -> Bool
implica True True = True
implica False False = True
implica _ _ = False

-- forma 2
implica2 :: Bool -> Bool -> Bool
implica2 a b = (a == b)


--- Ej. (d) ---
-- sumaGaussiana :: Integer -> Integer
-- que toma un entero no negativo y devuelve la suma de todos los enteros positivos
-- menores o iguales que ´el.
sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana (n - 1)

--- Ej. (e) ---
-- algunoEsCero :: (Integer, Integer, Integer) -> Bool,
-- que devuelve True si alguna de las componentes de la tupla es 0.
algunoEsCero :: (Integer, Integer, Integer) -> Bool
algunoEsCero (0, _, _) = True
algunoEsCero (_, 0, _) = True
algunoEsCero (_, _, 0) = True
algunoEsCero _ = False

-- Solución mejor
algunoEsCeroMejor :: (Integer, Integer, Integer) -> Bool
algunoEsCeroMejor (x, y, z) = (x * y * z == 0)

--- Ej. (f) ---
-- productoInterno :: (Float, Float) -> (Float, Float) -> Float,
-- que dados dos vectores v1 = (x1, y1), v2 = (x2, y2) ∈ R2, calcula su producto interno
-- v1 x v2 = x1x2 + y1y2.
productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

------ Segunda tanda ------

--- Ej. (1) ---
-- Implementar una función que, dado un número natural n, determine si puede escribirse
-- como suma de dos números primos: esSumaDeDosPrimos :: Integer -> Bool

-- esSumaDeDosPrimos :: Integer -> Bool
-- esSumaDeDosPrimos 

-- es Primo

esDivisible :: Integer -> Integer -> Bool
esDivisible n k = mod n k == 0

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n `esDivisible` k = k
                      | otherwise = menorDivisorDesde n (k + 1)


menorDivisor :: Integer -> Integer                      
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo n | n <= 1 = False
          | otherwise = (menorDivisor n == n)

sonAmbosPrimos :: Integer -> Integer -> Bool
sonAmbosPrimos a b = yLogico (esPrimo a) (esPrimo b)

esSumaDeDosPrimosAux :: Integer -> Integer -> Bool
esSumaDeDosPrimos _ 0 = False
esSumaDeDosPrimosAux a n | sonAmbosPrimos a n = True
                         | otherwise = esSumaDeDosPrimosAux (a + 1) (n - 1)

esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos n = esSumaDeDosPrimosAux 0 n

--- Ejercicio 2. ---
-- Conjetura (Christian Goldbach, 1742): todo n´umero par mayor que 2 puede escribirse
-- como suma de dos n´umeros primos. Escribir una funci´on que pruebe la conjetura hasta un
-- cierto punto. goldbach :: Integer -> Bool (hasta al menos 4.1018 deber´ıa ser cierto)
goldbach :: Integer -> Bool
goldbach n | n == 2 = True
           | esSumaDeDosPrimos n == False = False
           | otherwise = goldbach (n - 2)

--- Ejercicio 3. ---
-- Escribir una función que determine la suma de dígitos de un número positivo. Para esta
-- función pueden utilizar div y mod.