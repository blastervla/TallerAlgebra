--- Clase 5 ---

--- Ejercicios ---


--- Implementar una función eAprox :: Integer -> Float que aproxime el valor del número e
-- factorial
factorial :: Integer -> Integer
factorial n  | n == 0 = 1
             | otherwise = n + factorial (n - 1)

eAprox :: Integer -> Float
eAprox n | n == 0 = 1
         | n > 0 = (eAprox (n - 1)) + 1 / fromInteger(factorial n)

--- Definir la constante e :: Float como la aproximaci´on de e a partir de los primeros 100 términos de la serie anterior.
e :: Float
e = eAprox 100

---  Implementar una función parteEntera :: Float -> Integer que calcule la parte entera de un número real positivo.
parteEntera :: Float -> Integer
parteEntera x | x < 1 = 0
              | otherwise = (parteEntera (x - 1)) + 1

--- Cambiar la implementación de parteEntera :: Float -> Integer para que también funcione con números negativos.

parteEnterx :: Float -> Integer
parteEnterx x | (abs x) < 1 = 0
              | x > 0 = (parteEntera (x - 1)) + 1
              | x < 0 = (parteEntera (x + 1)) - 1

--- Implementar la siguiente funci´on
--- division :: Integer -> Integer -> (Integer, Integer)
--- Debe funcionar para a ≥ 0, d > 0 y no se pueden usar div, mod ni /.
sumaFst :: (Integer,Integer) -> (Integer,Integer)
sumaFst t = (fst t + 1, snd t)

division :: Integer -> Integer -> (Integer, Integer)
division a d | a < d = (0,a)
             | otherwise = sumaFst (division(a - d) d)


--- Extender la función division :: Integer -> Integer -> (Integer, Integer) 
--- para que funcione para a ∈ Z, d > 0.
divisione :: Integer -> Integer -> (Integer, Integer)
divisione a d | (abs a) < d = (0,a)
              | a <= 0 = sumaFst (division(a + d) d)
              | otherwise = sumaFst (division(a - d) d)


--- Implementar una función sumaDivisoresHasta :: Integer -> Integer -> Integer.
esDivisible :: Integer -> Integer -> Bool
esDivisible n k = mod n k == 0

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | n `esDivisible` k = k + sumaAnteriores
                       | otherwise = sumaAnteriores
                       where sumaAnteriores = sumaDivisoresHasta n (k - 1)

--- Implementar la función sumaDivisores en función de la anterior.
sumaDivisores n = sumaDivisoresHasta n n

--- Un entero p > 1 es primo si ningún natural k tal que 1 < k < p divide a p.
--- 1. menorDivisor :: Integer -> Integer que calcule el menor divisor (mayor que 1) de un natural n.

-- Forma manu (fea)
menorDivisor :: Integer -> Integer
menorDivisor n | n == 1 = 1 -- caso que rompe ;)
               | otherwise = aux n 1

aux n k | sumaDivisoresHasta n k > 1 = (sumaDivisoresHasta n k) - 1
        | otherwise = aux n (k + 1)


-- Forma linda
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n `esDivisible` k = k
                      | otherwise = menorDivisorDesde n (k + 1)

menorDivisor2 n = menorDivisorDesde n 2

--- 2. Implementar la función esPrimo :: Integer -> Bool.
-- Si el menor de los divisores de p es = a p, => p es primo
-- esPrimo :: Integer -> Bool

esPrimo :: Integer -> Bool
esPrimo n = (menorDivisor n == n)

--- 3 Implementar la siguiente función (sumatoria doble)

sumaA :: Integer -> Integer -> Integer -> Integer
sumaA i j m | m == 1 = i 
            | otherwise = i ^ m + sumaA i j (m - 1)
            
sumaB :: Integer -> Integer -> Integer -> Integer -> Integer
sumaB i j n m | n == 1 = sumaA 1 j m
              | otherwise = sumaA n j m + sumaB i j (n - 1) m

sumaCompleta :: Integer -> Integer -> Integer
sumaCompleta n m = sumaB 1 1 n m