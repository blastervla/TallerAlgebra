-- ====================== Ejercicios ====================== -- 
-- 1. Dado un entero, devuelve la unidad de ese entero:
unidades :: Integer -> Integer -- Versión posta
unidades n  | n > 0 = n `mod` 10
            | otherwise = (-n) `mod` 10

unidadesR :: Integer -> Integer -- Versión recursiva
unidadesR n     | n < 10 = n
                | otherwise = unidades (n - 10)

-- 2. Dados 3 enteros, devuelve la suma de dígitos de las unidades de los 3 números
sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
sumaUnidades3 a b c = (unidades a) + (unidades b) + (unidades c)

-- 3. Dados 3 números, determina si todos son impares
todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares a b c = (isOdd a) && (isOdd b) && (isOdd c)

isOdd :: Integer -> Bool
isOdd a = (mod a 2 == 1)

-- 4. Dados 3 números enteros determina si al menos uno de ellos es impar
alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar a b c = (isOdd a) || (isOdd b) || (isOdd c)

-- 5. Dados 3 números enteros determina si al menos dos de ellos son impares
alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares a b c = ((isOdd a) && (isOdd b)) || ((isOdd b) && (isOdd c)) || ((isOdd a) && (isOdd c))

alMenosDosImparesAlt :: Integer -> Integer -> Integer -> Bool
alMenosDosImparesAlt a b c = a `mod` 2 + b `mod` 2 + c `mod` 2 >= 2

-- 6. Dados 3 números enteros determina si al menos dos de ellos son pares
alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares a b c = not (alMenosDosImpares a b c)

-- ====================== Parte 2 - Ejercicios ====================== -- 
-- 7. Dados dos enteros, implementar funciones (r1, r2, r3) que determinen si a ~ b (~ = 'parecido') donde:
--    i. a ~ b si tienen la misma paridad
--   ii. a ~ b si 2a + 3b es divisible por 5
--  iii. a ~ b si los dígitos de las unidades de a, b y a*b son todos distintos
r1 :: Integer -> Integer -> Bool
r1 a b = isOdd a == isOdd b

r2 :: Integer -> Integer -> Bool
r2 a b = (2*a + 3*b) `mod` 5 == 0

r3 :: Integer -> Integer -> Bool
r3 a b = unidades a /= unidades b && unidades b /= unidades (a * b) && unidades a /= unidades (a * b)

-- 8. Se define en 𝑹 (reales) la relación de equivalencia asociada a la partición: 𝑹 = (-∞; 3) U [3; +∞)
--    Determinar el tipo e implementar una función que dados dos números x, y ∈ 𝑹 determine si x ~ y
sonEquivalentes8 :: Float -> Float -> Bool
sonEquivalentes8 a b = (a < 3 && b < 3) || (a >= 3 && b >= 3)

-- 9. Repetir el ejercicio anterior para la partición: 𝑹 = (-∞; 3) U [3; 7) U [7; +∞)
--    Determinar el tipo e implementar una función que dados dos números x, y ∈ 𝑹 determine si x ~ y
sonEquivalentes9 :: Float -> Float -> Bool
sonEquivalentes9 a b = (a < 3 && b < 3) || (perteneceParticion9_2 a && perteneceParticion9_2 b) || (a >= 7 && b >= 7)

perteneceParticion9_2 :: Float -> Bool
perteneceParticion9_2 n = (n >= 3 && n < 7)

-- 10. Dados (a, b) y (p, q) en 𝒁 × 𝒁 − {(0, 0)}, determinar el tipo e implementar funciones que
--     determinen si (a, b) ∼ (p, q) para las siguientes relaciones:
--       i. (a, b) ∼ (p, q) si existe k ∈ 𝒁 tal que (a, b) = k(p, q)
--      ii. (a, b) ∼ (p, q) si existe k ∈ 𝑹 tal que (a, b) = k(p, q)
equivalenciaEnZ :: (Integer, Integer) -> (Integer, Integer) -> Bool
equivalenciaEnZ ab pq = (fst ab) `mod` (fst pq) == 0 && (snd ab) `mod` (snd pq) == 0

equivalenciaEnR :: (Integer, Integer) -> (Integer, Integer) -> Bool
equivalenciaEnR ab pq = equivalenciaEnZ ab pq || equivalenciaEnZ pq ab

-- ====================== Parte 3 - Ejercicios ====================== -- 
-- Recursividad:

-- 11. Implementar la función sc :: Integer -> Integer definida por:
-- sc(n) = 0 si n == 0
-- sc(n) = sc(n-1) + n² si no
sc :: Integer -> Integer
sc n    | n == 0 = 0
        | otherwise = sc (n - 1) + n^2

-- 12. Implementar la función fib :: Integer -> Integer que devuelve el i-ésimo número de
-- Fibonacci. Recordar que la sucesión de Fibonacci se define como:
-- fib(n) = 1 si n == 0
-- fib(n) = 1 si n == 1
-- fib(n) = fib(n-1) + fib(n - 2) si no
fib :: Integer -> Integer
fib n   | n == 0 || n == 1 = 1
        | otherwise = fib (n - 1) + fib (n - 2)

-- 13. Implementar funciones recursivas para calcular el n-ésimo término de las siguientes
-- sucesiones del Ejercicio 16 y 20 de la Práctica 2.
--   i. a1 = 2, a(n+1) = 2n * a(n) + 2^(n+1) * n!, para todo n ∈ 𝑵
--  ii. a1 = 1, a2 = 2 y a(n+2) = n*a(n+1) + 2(n + 1)a(n), para todo n ∈ 𝑵
fact :: Integer -> Integer
fact n  | n == 0 = 1
        | otherwise = n + fact (n - 1)

ai :: Integer -> Integer
ai n    | n == 1 = 2
        | otherwise = 2 * (n - 1) * (ai (n - 1)) + 2^(n) * (fact (n-1))

-- TODO: Los dos que faltan :P