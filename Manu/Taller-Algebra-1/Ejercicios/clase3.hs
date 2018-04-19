--- Ejercicio 1 ---
-- unidades: dado un entero, devuelve el d´ıgito de las unidades del n´umero (el digito menos significativo).

unidades :: Integer -> Integer
unidades n = mod n 10

-- Equivale a
unidadesLindo n = n `mod` 10

-- Correcto
unidadesCorrecto n | n > 0 = mod n 10
                   | otherwise = mod (-n) 10


--- Ejercicio 2 ---
-- sumaUnidades3: dados 3 enteros, devuelve la suma de los digitos de las unidades de los 3 numeros

sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
sumaUnidades3 a b c = (unidades a) + (unidades b) + (unidades c)

--- Ejercicio 3 ---
-- todosImpares: dados 3 n´umeros enteros determina si son todos impares

esImpar :: Integer -> Bool
esImpar n = (mod n 2 == 1)

todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares a b c = esImpar a && esImpar b && esImpar c

--- Ejercicio 4 ---
-- alMenosUnImpar: dados 3 numeros enteros determina si al menos uno de ellos es impar

alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar a b c = esImpar a || esImpar b || esImpar c

--- Ejercicio 5 ---
-- alMenosDosImpares: dados 3 numeros enteros determina si al menos dos de ellos son impares.

alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares a b c | esImpar a = esImpar b || esImpar c
                        | otherwise = esImpar b && esImpar c

-- Tambien puede ser sumando los restos
alMenosDosImparesResto a b c = mod a 2 + mod b 2 + mod c 2 >= 2

--- Ejercicio 6 ---
-- alMenosDosPares: dados 3 numeros enteros determina si al menos dos de ellos son pares.

esPar :: Integer -> Bool
esPar n = not (esImpar n)

alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares a b c | esPar a = esPar b || esPar c
                      | otherwise = esPar b && esPar c

alMenosDosParesVladi a b c = not (alMenosDosImpares a b c)

-- ==================== Relaciones ==================== --

--- Ejercicio 7 ---
-- Dados dos enteros a, b implementar funciones:
-- (r1, r2 y r3) :: Integer -> Integer -> Bool que determinen si a ∼ b donde:

-- 1 a ∼ b si tienen la misma paridad
r1 :: Integer -> Integer -> Bool
r1 a b = (esPar a == esPar b)

-- 2 a ∼ b si 2a + 3b es divisible por 5
r2 :: Integer -> Integer -> Bool
r2 a b = (mod (2*a + 3*b) 5 == 0)

-- 3 a ∼ b si los digitos de las unidades de a, b y ab son todos distintos
r3 :: Integer -> Integer -> Bool
r3 a b = (unidades a /= unidades b) && (unidades b /= unidades (a*b))
-- tambien contempla el caso unidades a /= unidades (a*b) por transitividad
-- a != b y b != b*a => a != b*a

--- Ejercicio 8 ---
-- Se define en R la relacion de equivalencia asociada a la particion
-- R = (−∞, 3) ∪ [3, +∞)
-- Determinar el tipo e implementar una funcion que dados dos numeros x, y ∈ R determine si x ∼ y.

esEquivalente8 :: Float -> Float -> Bool
esEquivalente8 x y | ((abs x) < 3) = ((abs y) < 3)
                   | ((abs x) >= 3) = ((abs y) >= 3)

--- Ejercicio 9 ---
-- Repetir el ejercicio anterior para la particion
-- R = (−∞, 3) ∪ [3, 7) ∪ [7, +∞)

esEquivalente9 :: Float -> Float -> Bool
esEquivalente9 x y | ((abs x) < 3) = (abs y) < 3
                   | ((abs x) >= 3 && x < 7) = ((abs y) >= 3 && (abs y) < 7)
                   | ((abs x) >= 7) = ((abs y) >= 7)