yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _ = False

oLogico :: Bool -> Bool -> Bool
oLogico False False = False
oLogico _ _ = True

implica :: Bool -> Bool -> Bool
implica False False = True
implica True True = True
implica _ _ = False

sumaGaussiana :: Integer -> Integer
sumaGaussiana 1 = 1
sumaGaussiana n = n + sumaGaussiana (n - 1)

algunoEsCero :: (Integer, Integer, Integer) -> Bool
algunoEsCero (0, _, _) = True
algunoEsCero (_, 0, _) = True
algunoEsCero (_, _, 0) = True
algunoEsCero _ = False

productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- Recursión (Again!)

-- Auxiliares ----------------------
--          dividendo -> divisor -> (cociente, resto)
division :: Integer -> Integer -> (Integer, Integer)
division a d    | a < d = (0, a)
                | otherwise = (1 + fst (division (a - d) d) , snd (division (a - d) d))

divisionEntera :: Integer -> Integer -> (Integer, Integer)
divisionEntera a d  | a < 0 = ((-1) * fst (mNegDiv), (-1 * snd (mNegDiv)))
                    | otherwise = division a d
                    where mNegDiv = division ((-1) * a) d

esDivisor :: Integer -> Integer -> Bool
esDivisor a d = snd (divisionEntera a d) == 0

menorDivisorAux :: Integer -> Integer -> Integer -> Integer
menorDivisorAux actual n menorHastaAhora    | actual == 1 = menorHastaAhora
                                            | esDivisor n (actual) = menorDivisorAux (actual - 1) n actual
                                            | otherwise = menorDivisorAux (actual - 1) n menorHastaAhora

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorAux n n n

esPrimo :: Integer -> Bool
esPrimo n   | n > 1 = menorDivisor n == n
            | otherwise = False
-- Auxiliares ----------------------
siguientePrimoMenor :: Integer -> Integer
siguientePrimoMenor 2 = 2
siguientePrimoMenor n   | esPrimo (n - 1) = n - 1
                        | otherwise = siguientePrimoMenor (n - 1)

esSumaDeDosPrimosAux :: Integer -> Integer -> Bool
esSumaDeDosPrimosAux n 2 = esPrimo (n - 2)
esSumaDeDosPrimosAux n k = esPrimo (n - k) || esSumaDeDosPrimosAux n (siguientePrimoMenor k)

esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos n | n > 2 = esSumaDeDosPrimosAux n (siguientePrimoMenor n)
                    | otherwise = False

conjeturaDeGoldbachAux :: Integer -> Bool
conjeturaDeGoldbachAux 4 = True
conjeturaDeGoldbachAux n = esSumaDeDosPrimos n && conjeturaDeGoldbachAux (n - 2)

conjeturaDeGoldbach :: Bool
conjeturaDeGoldbach = conjeturaDeGoldbachAux (4 * 10^18)

sumaDeDigitos :: Integer -> Integer
sumaDeDigitos 0 = 0
sumaDeDigitos n = n `mod` 10 + sumaDeDigitos (n `div` 10)

cantDigitos :: Integer -> Integer
cantDigitos 0 = 0
cantDigitos n = 1 + cantDigitos (n `div` 10)

todosLosDigitosSonIguales :: Integer -> Bool
todosLosDigitosSonIguales n = sumaDeDigitos n - (n `mod` 10) * cantDigitos n == 0

collatz :: Integer -> Integer
collatz anterior    | anterior `mod` 2 == 0 = anterior `div` 2
                    | otherwise = 3 * anterior + 1

collatzHasta1 :: Integer -> Integer -> Integer
collatzHasta1 n terminos    | collatz n == 1 = terminos + 1
                            | otherwise = collatzHasta1 (collatz n) (terminos + 1)

terminosCollatz :: Integer -> Integer
terminosCollatz n = collatzHasta1 n 1

maxCollatzIndexed :: Integer -> Integer -> Integer
maxCollatzIndexed x 1 = x
maxCollatzIndexed max index | terminosCollatz max >= terminosCollatz (index) = maxCollatzIndexed max (index - 1)
                            | otherwise = maxCollatzIndexed (index) (index - 1)

-- Para un número máximo, devuelve el número < n máx que tiene más términos en la sucesión de collatz
maxCollatz :: Integer -> Integer
maxCollatz n = maxCollatzIndexed n n

