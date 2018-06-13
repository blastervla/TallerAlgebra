type Complejo = (Float, Float)

-- ========================
-- ||      Bloque 1      ||
-- ========================

-- 1 suma :: Complejo -> Complejo -> Complejo
suma :: Complejo -> Complejo -> Complejo
suma (a, b) (c, d) = (a + c, b + d) 

-- 2 producto :: Complejo -> Complejo -> Complejo
producto :: Complejo -> Complejo -> Complejo
producto (a, b) (c, d) = ((a*c) - (b*d), (a*d + b*c))

-- 3 re :: Complejo -> Float, im :: Complejo -> Float
re :: Complejo -> Float
re (a, _) = a

im :: Complejo -> Float
im (_, b) = b

-- 4 modulo :: Complejo -> Float
modulo :: Complejo -> Float
modulo (a, b) = sqrt (a*a + b*b)

-- 5 argumento :: Complejo -> Float
argumento :: Complejo -> Float
argumento (a, b) = atan2 b a

-- ========================
-- ||      Bloque 2      ||
-- ========================

-- 1 conjugado :: Complejo -> Complejo
conjugado :: Complejo -> Complejo
conjugado (a, b) = (a, (-b))

-- 2 inverso :: Complejo -> Complejo
cocienteConReal :: Complejo -> Float -> Complejo
cocienteConReal z x = producto z (1/x, 0)

cuadrado :: Float -> Float
cuadrado a = a*a

inverso :: Complejo -> Complejo
inverso z = cocienteConReal (conjugado z) (cuadrado (modulo z))

-- 3 cociente :: Complejo -> Complejo -> Complejo
cociente :: Complejo -> Complejo -> Complejo
cociente z w = producto z (inverso w)

-- 4 potencia :: Complejo -> Integer -> Complejo
potencia :: Complejo -> Integer -> Complejo
potencia z 0 = (1, 0)
potencia z n | n < 0 = inverso (potencia z (-n))
             | otherwise = producto z (potencia z (n-1))

-- ========================
-- ||      Bloque 3      ||
-- ========================

numerador = (1, sqrt 3) :: Complejo
denominador = (1, -1) :: Complejo
a = (-1, sqrt 3) :: Complejo

-- 1 El ejercicio 6 i) de la pr´actica 6 pide encontrar la forma binomial de 
-- Encontrarla como una expresi´on usando las constantes anteriores
resultado = potencia (cociente numerador denominador) 17 :: Complejo

-- 2 El ejercicio 6 ii) pide encontrar la forma binomial de (−1 + √
-- 3i)
-- n para cada n ∈ N. Hacer
-- una funci´on primerasPotencias :: Integer -> [Complejo] que dado un natural n
-- devuelva la lista de las primeras n potencias de a desde 1 hasta n.
primerasPotencias :: Integer -> [Complejo]
primerasPotencias 1 = [potencia a 1]
primerasPotencias n = (potencia a n) : primerasPotencias (n-1)