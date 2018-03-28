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