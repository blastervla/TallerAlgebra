mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

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

mcd2 :: Integer -> Integer -> Integer
mcd2 1 _ = 1
mcd2 a b    | esDivisor b (menorDivisor a) = (menorDivisor a) * (mcd2 (a `div` (menorDivisor a)) (b `div` (menorDivisor a)))
            | otherwise = mcd2 (a `div` (menorDivisor a)) b


emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a 0 = (a, 1, 0)
emcd a b = (g, t1, (s1 - t1 * (a `div` b)))
            where (g, s1, t1) = emcd b (a `mod` b)


tieneSolucion :: Integer -> Integer -> Integer -> Bool
tieneSolucion a b m = esDivisor b (mcd a m)

solucionParticular :: Integer -> Integer -> Integer -> Integer
solucionParticular a b m | esDivisor b (mcd a m) = (m `div` a) * (b `div` (mcd a m))


-- ax -= b (mod m) <=> ax + mk = b

-- (a : m) | b => (a : m)x = 0 (b)

-- (a : m)k = b

-- ax -= (a : m) (mod m)
-- 3x -= (3 : 7) (mod 7)
-- 3x -= 1 (mod 7)
-- x = 7 `div` 3 = m `div` a




