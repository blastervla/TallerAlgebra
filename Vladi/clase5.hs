fact :: Integer -> Integer
fact n  | n == 0 = 1
        | otherwise = n + fact (n - 1)

eAprox :: Integer -> Float
eAprox n    | n == 0 = 1
            | otherwise = 1/(fromInteger (fact n)) + eAprox (n - 1)

e :: Float
e = eAprox 100

parteEntera :: Float -> Integer
parteEntera n   | -1 < n && n < 1 = 0
                | n >= 1 = (parteEntera (n - 1)) + 1
                | n <= -1 = (parteEntera (n + 1)) - 1

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

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n tope   | (n - 1) == 1 = 1
                            | esDivisor tope (n - 1) = n - 1 + sumaDivisoresHasta (n - 1) tope
                            | otherwise = sumaDivisoresHasta (n - 1) tope

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n

menorDivisorAux :: Integer -> Integer -> Integer -> Integer
menorDivisorAux actual n menorHastaAhora    | actual == 1 = menorHastaAhora
                                            | esDivisor n (actual) = menorDivisorAux (actual - 1) n actual
                                            | otherwise = menorDivisorAux (actual - 1) n menorHastaAhora

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorAux n n n

esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n

sumatoria :: Integer -> Integer -> Integer
sumatoria k n   | n == 1 = n
                | otherwise = k^n + sumatoria k (n - 1)

sumatoriaDoble :: Integer -> Integer -> Integer
sumatoriaDoble n m  | n == 1 = sumatoria 1 m
                    | otherwise = sumatoria n m + sumatoriaDoble (n - 1) m

sumaPotenciasSimples :: Integer -> Integer -> Integer -> Integer
sumaPotenciasSimples q a b  | b == 1 = q ^ (a + 1)
                            | otherwise = q ^ (a + b) + sumaPotenciasSimples q a (b - 1)

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m | n == 1 = sumaPotenciasSimples q 1 m
                    | otherwise = sumaPotenciasSimples q n m + sumaPotenciasSimples q (n - 1) m

sumaRacionalesSimple :: Integer -> Integer -> Float
sumaRacionalesSimple p q | q == 1 = fromInteger p
                        | otherwise = (fromInteger p) / (fromInteger q) + sumaRacionalesSimple p (q - 1)

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n m  | n == 1 = sumaRacionalesSimple 1 m
                    | otherwise = sumaRacionalesSimple n m + sumaRacionalesSimple (n - 1) m