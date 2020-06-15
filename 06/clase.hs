{- | Implementar sumatoria :: [Int] -> Int que indique la suma de los elementos de una lista.-}
{- sumatoria sin pattern matching.
sumatoria :: [Int] -> Int
sumatoria l | l == [] = 0
            | otherwise = head l + sumatoria (tail l) -}

{- sumatoria con pattern matching.-}
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

{- | Implementar longitud :: [Int] -> Int que indique la cantidad de elementos de una lista.-}
{- longitud con pattern matching.
longitud :: [Int] -> Int 
longitud l | l == [] = 0
            | otherwise = 1 + longitud (tail l) -}

{- longitud con pattern matching.-}
longitud :: [Int] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

{- | Implementar pertenece :: Int -> [Int] -> Bool que indique si un elemento aparece en la lista.-}
{- pertenece sin pattern matching.
pertenece :: Int -> [Int] -> Bool
pertenece n l | l == [] = False
                | otherwise = head l == n || pertenece n (tail l) -}

{- pertenece con pattern matching. -}
pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece n (x:xs) = x == n || pertenece n xs 

{- | Implementar primerMultiploDe45345 :: [Int] -> Int que indique el primer elemento de la lista que es multiplo de 45345
que encuentre en la lista.-}
primerMultiploDe45345 :: [Int] -> Int
primerMultiploDe45345 l | mod (head l) 45345 == 0 = (head l)
                        | otherwise = primerMultiploDe45345 (tail l)