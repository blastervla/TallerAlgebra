--- Ejercicio 1 ---
-- Implementar la función menorLex :: (Float, Float, Float) -> (Float, Float, Float) -> Bool que dados dos
-- vectores x, y ∈ R
-- 3 decida si x es menor a y en el sentido lexicográfico.
-- Por ejemplo:
-- menorLex (3,-1,2) (5,10,0) True (pues 3 < 5)
-- menorLex (4,-1,7) (4,21,5) True (pues coinciden en la primera coordenada, y −1 < 21)
-- menorLex (2,1,31) (2,1,-5) False (pues coinciden en las dos primeras coordenadas, pero 31 ≮ −5)

menorLex :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
menorLex (x1, x2, x3) (y1, y2, y3) 
    | x1 < y1 = True
    | x1 > y1 = False
    | x2 < y2 = True
    | x2 > y2 = False
    | x3 < y3 = True
    | x3 > y3 = False

--- Ejercicio 2 ---
-- Implementar una función sumaFibonacci :: Integer -> Integer que para cada n ≥ 1 calcule Pn
-- j=0 fj , donde fn
-- es el n–ésimo término de la sucesión de Fibonacci.
-- Por ejemplo:
-- sumaFibonacci 2 1+1+2 4
-- sumaFibonacci 4 1+1+2+3+5 12

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

sumaFibonacci :: Integer -> Integer
sumaFibonacci 1 = fib 0 + fib 1
sumaFibonacci n = fib n + sumaFibonacci (n - 1)

--- Ejercicio 3 ---
-- Implementar la funcion esDefectivo :: Integer -> Bool que dado un n ∈ N>0 determine si es defectivo, lo cual
-- vale si y solo si la suma de los divisores propios positivos1 de n es menor que n.
-- Por ejemplo:
-- esDefectivo 16 True
-- esDefectivo 12 False
-- ya que la suma de los divisores propios de 16 es 1 + 2 + 4 + 8 = 15, que es menor que 16; y la suma de los divisores
-- propios de 12 es 1 + 2 + 3 + 4 + 6 = 16, que no es menor que 12.

esDivisorDe :: Integer -> Integer -> Bool
esDivisorDe a b = b `mod` a == 0

sumaDivisoresPropiosDesde :: Integer -> Integer -> Integer
sumaDivisoresPropiosDesde _ 1 = 1
sumaDivisoresPropiosDesde n i 
    | i `esDivisorDe`n = i + sumaAnteriores
    | otherwise = sumaAnteriores
    where sumaAnteriores = sumaDivisoresPropiosDesde n (i - 1)

sumaDivisoresPropios :: Integer -> Integer
sumaDivisoresPropios n = sumaDivisoresPropiosDesde n (n - 1)

esDefectivo :: Integer -> Bool
esDefectivo n = n > sumaDivisoresPropios n

--- Ejercicio 4 ---
-- Programe la función maximaDistancia :: [Integer] -> Integer, que determina cuál es la máxima distancia entre
-- dos elementos consecutivos en una lista de números enteros.
-- Por ejemplo:
-- maximaDistancia [1,6,2,7,8] 5 (la máxima distancia es entre 1 y 6)
-- maximaDistancia [1,6,2,7,1] 6 (la máxima distancia es entre 7 y 1)
-- maximaDistancia [1,5,-10,3] 15 (la máxima distancia es entre 5 y −10)
-- Aclaración: Puede asumir que la lista tiene al menos dos elementos.

max' :: Integer -> Integer -> Integer
max' a b | a > b = a
         | otherwise = b

abs' :: Integer -> Integer
abs' n | n >= 0 = n
       | otherwise = (-n)

dist :: Integer -> Integer -> Integer
dist a b | a < b = dist b a
         | otherwise = abs' (a - b)

maximaDistancia :: [Integer] -> Integer
maximaDistancia [a,b] = dist a b
-- maximaDistancia (x:(x':[])) = dist a b (otro posible caso base)
maximaDistancia (x:(x':xs)) = max' (dist x x') (maximaDistancia (x':xs))

--- Ejercicio 5 ---
-- Implementar la función sonAmigos :: Integer -> Integer -> Bool que dados dos números naturales mayores a
-- cero determine si son amigos, lo cual vale si y solo si la suma de los divisores propios de uno es igual al otro número
-- y viceversa.
-- Por ejemplo:
-- sonAmigos 220 284 True
-- ya que la suma de los divisores propios de 220 es 1 + 2 + 4 + 5 + 10 + 11 + 20 + 22 + 44 + 55 + 110 = 284 y la suma de
-- los divisores propios de 284 es 1 + 2 + 4 + 71 + 142 = 220.

-- Utilizo la función sumaDivisoresPropios definida en el ejercicio 3

sonAmigos :: Integer -> Integer -> Bool
sonAmigos a b = (sumaDivisoresPropios a == b) && (sumaDivisoresPropios b == a)