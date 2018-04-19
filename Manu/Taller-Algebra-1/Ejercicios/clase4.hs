--- Ejercicio 1 ---
-- Implementar la funcion fib :: Integer -> Integer que devuelve el i-esimo numero de Fibonacci. 
-- Recordar que la secuencia de Fibonacci se define como:
--            1 si n = 0
-- fib(n) =   1 si n = 1
--            fib(n − 1) + fib(n − 2) en otro caso

fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n - 1) + fib (n - 2)


--- Sumatorias ---
sumatoria :: Integer -> Integer
sumatoria n | n > 1 = n + sumatoria (n - 1)
            | n == 1 = 1

--- Ej 1 ---
-- sum 2^i from i = 0 to n
s1 :: Integer -> Integer
s1 n | n == 0 = 1
     | n > 0 = 2 ^ n + s1 (n-1)

--- Ej 2 ---
-- sum q^i from i = 1 to n, n€No y q€R
s2 :: Integer -> Float -> Float
s2 n q | n == 0 = 0
       | n == 1 = q
       | n > 1 = q ^ n + s2 (n-1) q

--- Ej 3 ---
s3 :: Integer -> Float -> Float
s3 n q = s2 (2*n) q

--- Ej 4 ---
-- sum q ^ i / 2 from i=n to 2n === sum q ^(i+n) / 2 from i=0 to n
s4 :: Integer -> Float -> Integer -> Float
s4 n q k | n == 0 = q ^ k / 2
         | n > 0 = ((q^(k+n)) / 2 )+ (s4 (n-1) q k)