-- ========================
-- ||      Bloque 1      ||
-- ========================

--- Ej. 1 ---
-- Programar la funci´on
-- mcd :: Integer -> Integer -> Integer
-- que calcule el m´aximo com´un divisor entre dos n´umeros utilizando el algoritmo de Euclides.
-- mcd a b debe funcionar siempre que a > 0, b ≥ 0.
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

--- Ej. 2 ---
-- Pensar otro algoritmo para calcular el m´aximo com´un divisor modificando la funci´on
-- menorDivisor programada en clases anteriores

-- Menor divisor
esDivisible :: Integer -> Integer -> Bool
esDivisible n k = mod n k == 0

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n `esDivisible` k = k
                      | otherwise = menorDivisorDesde n (k + 1)

menorDivisor :: Integer -> Integer                      
menorDivisor n = menorDivisorDesde n 2

-- Maximo común divisor
mcd' a 1 = 1
mcd' a b | mod a mDb == 0 = (mDb) * mcd' (div a mDb) (div b mDb)
         | otherwise = mcd a (div b mDb)
        where mDb = menorDivisor b
-- guarda ubicada materia recursada
-- guarda que veo parcial que meo
-- guarda que observo parcial que recuerdo
-- guarda que ubico código que modifico

-- ========================
-- ||      Bloque 2      ||
-- ========================
-- Programar la funci´on
-- emcd :: Integer -> Integer -> (Integer, Integer, Integer)
-- que utilice el algoritmo de Euclides extendido para obtener una 3-upla (g, s,t) tal que
-- g = (a : b) = sa + tb.

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a 0 = (a, 1, 0)
emcd a b = (g, t1, s1 - t1 * q)
         where (g, s1, t1) = emcd b (a `mod` b)
               q = div a b

-- Utilizando la funci´on emcd encontrar alguna soluci´on de las ecuaciones diof´anticas
--- 1. 29x + 17y = 1
-- x = -7
-- y = 12

--- 2. 89x + 23y = 1
-- x = -8
-- y = 31

--- 3. 93x + 27y = 3
-- x = -2
-- y = 7

-- SI no se igualasen al mcd entre ambos, multiplico para llegar a el

-- ========================
-- ||      Bloque 3      ||
-- ========================

-- ax ≡ b (mod m)
-- ¿Cuándo tiene solución la ecuación?
-- Recordar que esa ecuacion es equivalente a la ecuacion diofantica
-- ax + mk = b
-- Tiene solucion si y solo si (a : m) divide a b

--- Ej. 1 ---
-- Programar la funcion
-- tieneSolucion :: Integer -> Integer -> Integer -> Bool
-- que dados a, b y m determine si la ecuaci´on ax ≡ b (mod m) tiene solución.
esDivisorDe :: Integer -> Integer -> Bool
esDivisorDe a b = b `mod` a == 0

tieneSolucion :: Integer -> Integer -> Integer -> Bool
tieneSolucion a b m = (mcd a m) `esDivisorDe` b

--- Ej. 2 ---
-- Programar la funcion
-- solucionParticular :: Integer -> Integer -> Integer -> Integer
-- que dados a, b y m determine, si existe, un entero x tal que ax ≡ b (mod m). (La funci´on
-- puede quedar indefinida si la ecuacion no tiene solucion.)
-- Sugerencia: encontrar primero una solucion de ax ≡ (a : m) (mod m)

solucionDiofanticaFeliz :: Integer -> Integer -> Integer -> (Integer, Integer)
solucionDiofanticaFeliz a b c | c == g = (s, t)
                              where (g, s, t) = emcd a b