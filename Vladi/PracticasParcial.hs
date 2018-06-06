-- 1.
elDelMedio :: Integer -> Integer -> Integer -> Integer
elDelMedio a b c    | (a >= b && a <= c) || (a <= b && a >= c) = a
                    | (b >= a && b <= c) || (b <= a && b >= c) = b
                    | otherwise = c

-- 2.
sonCongruentesModulo :: Integer -> Integer -> Integer -> Bool
sonCongruentesModulo a b m  | a < b = False
                            | a == b = True
                            | otherwise = sonCongruentesModulo (a - m) b m

-- 3.
sucesion :: Integer -> Float
sucesion 1 = 2
sucesion n = 2 + 1 / (sucesion (n-1))

raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = (sucesion n) - 1

-- 4.
sqrt_n :: Integer -> Integer
sqrt_n n = floor (sqrt (fromIntegral n))

esSumaDePerfectos :: Integer -> Bool
esSumaDePerfectos n = primerNumero^2 + segundoNumero^2 == n
                    where   primerNumero = sqrt_n n
                            segundoNumero = sqrt_n (n - primerNumero^2)

-- 5.
cantDigitos :: Integer -> Integer
cantDigitos n   | n < 10 = 1
                | otherwise = 1 + cantDigitos (n `div` 10)

ultimoDigito :: Integer -> Integer
ultimoDigito n  | n < 10 = n
                | otherwise = ultimoDigito (n `div` 10)

intermedios :: Integer -> Integer
intermedios n   | n < 100 = 0
                | otherwise = sinPrimero - 10^((cantDigitos sinPrimero) - 1) * (ultimoDigito sinPrimero)
                            where sinPrimero = n `div` 10

esCapicua :: Integer -> Bool
esCapicua n | n < 10 = True
            | otherwise = (n `mod` 10) == (ultimoDigito n) && esCapicua (intermedios n)



-------------------------- Simulacro --------------------------
--1.
menorLex :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
menorLex (a, b, c) (x, y, z)    | a == x && b == y = c < z
                                | a == x = b < y
                                | otherwise = a < x

-- 2.
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

sumaFibonacci :: Integer -> Integer
sumaFibonacci 0 = 1
sumaFibonacci n = fib n + sumaFibonacci (n - 1)

-- 3.
sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta _ 1 = 1
sumaDivisoresHasta n i  | n `mod` i == 0 = i + sumaDivisoresHasta n (i - 1)
                        | otherwise = sumaDivisoresHasta n (i - 1)

esDefectivo :: Integer -> Bool
esDefectivo n = sumaDivisoresPropios < n
                where sumaDivisoresPropios = sumaDivisoresHasta n (n - 1)

-- 4.
mayor :: Integer -> Integer -> Integer
mayor a b   | a > b = a
            | otherwise = b

maximaDistancia :: [Integer] -> Integer
maximaDistancia (x:xs)  | length xs == 1 = abs ((head xs) - x)
                        | otherwise = mayor (abs ((head xs) - x)) (maximaDistancia xs)

-- 5.
sonAmigos :: Integer -> Integer -> Bool
sonAmigos a b = sumaDivisoresHasta a (a - 1) == b && sumaDivisoresHasta b (b - 1) == a