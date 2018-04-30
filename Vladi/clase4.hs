f1 :: Integer -> Integer
f1 n    | n == 0 = 1
        | otherwise = 2^n + f1 (n-1)

esPar :: Integer -> Bool
esPar n     | n > 1 = esPar (n - 2)
            | otherwise = (n == 0)

esMultiplo :: Integer -> Integer -> Bool
esMultiplo n q  | n > (q - 1) = esMultiplo (n - q) q
                | otherwise = (n == 0)

sumaImpares :: Integer -> Integer
sumaImpares n   | n == 1 = 1
                | otherwise = n_esimoImpar + sumaImpares (n-1)
                where n_esimoImpar = (2 * n) - 1