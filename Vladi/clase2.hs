signo n | n > 0 = 1
		| n == 0 = 0
		| n < 0 = -1

absoluto n 	| n >= 0 = n
			| n < 0 = -n

absolutoAlternativo n = (signo n) * n

maximo n m 	| n > m = n
			| otherwise = m

maximo3 n m o 	| n > m = (maximo n o)
				| otherwise = (maximo m o) -- Collapses case m > n && n == m, as in both we would return the same

maximo3Opt n m o = (maximo (maximo n m) o) -- Más compacto

romperNumerosFlotantes n 	| ((sqrt n)**2) == n = True -- Probar esta función con Pi (3.141592654)
							| otherwise = False

funcionParamsDistintoTipo :: Integer -> Integer -> Bool -> Bool
funcionParamsDistintoTipo x y b = b || (x > y)

esPar :: Integer -> Bool
esPar n = (mod n 2) == 0

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe n m = (mod n m) == 0

crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)

invertir :: (a, b) -> (b, a)
invertir x = (snd x, fst x)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos p q = (sqrt ((((fst p) - (fst q))^2) + (((snd p) - (snd q))^2)))

f1 :: Float -> (Float, Float, Float) -- f1 del Ejercicio 32 de la práctica 1
f1 x = (2 * x, x^2, x - 7)

f2 :: Integer -> Integer -- f2 del Ejercicio 32 de la práctica 1
f2 n 	| (esPar n) = div n 2
					| otherwise = n + 1

f3 :: Integer -> Integer -- f del Ejercicio 33 de la práctica 1
f3 n 	| (mod n 6) == 0 = div (n^2) 2
				| otherwise = 3*n + 1

g :: (Integer, Integer) -> Integer -- g del Ejercicio 33 de la práctica 1
g n = (fst n)*((snd n) + 1)

compfg :: (Integer, Integer) -> Integer -- f(g(n))
compfg n = (f3 (g n))