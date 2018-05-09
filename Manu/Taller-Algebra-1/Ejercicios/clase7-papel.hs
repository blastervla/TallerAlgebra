--- Ejercicio 1 ---
--- Tipar y evaluar las siguientes expresiones

-- head [(1,2), (3,4), (5,2)]
head [(1,2), (3,4), (5,2)] :: (Integer, Integer)
head [(1,2), (3,4), (5,2)] = (1,2)

-- tail [1,2,3,4,4,3,2,1]
tail [1,2,3,4,4,3,2,1] :: [Integer]
tail [1,2,3,4,4,3,2,1] = [2,3,4,4,3,2,1]

-- head []
explota.

-- head [1,2,3] : [2,3]
head [1,2,3] : [2,3] :: [Integer]

head [1,2,3] = 1
1 : [2,3] = [1,2,3]

head [1,2,3] : [2,3] = [1,2,3]

-- [True, True] ++ [False, False]
[True, True] ++ [False, False] :: [Bool]
[True, True] ++ [False, False] = [True, True, False, False]

-- [1,2] : []
[1,2] : [] :: [[Integer]]
[1,2] : [] = [[1,2]]
