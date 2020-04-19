maximo :: Int -> Int -> Int
maximo x y | x >= y = x
           | otherwise = y

absoluto :: Int -> Int
absoluto x = abs x

{- maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | abs x > abs y = abs x
                   | otherwise = abs y -}

maximoAbsoluto :: Int -> Int -> Int 
maximoAbsoluto x y = maximo (abs x) (abs y)

{- maximoAbsoluto3 :: Int -> Int -> Int -> Int
maximoAbsoluto3 x y z | (abs x > abs y) && (abs x > abs z) = abs x
                      | (abs y > abs x) && (abs y > abs z) = abs y
                      | otherwise = abs y -}

maximoAbsoluto3 :: Int -> Int -> Int -> Int
maximoAbsoluto3 x y z = maximo (maximo (abs x) (abs y)) (abs z)

{- algunoEsCero :: Float -> Float -> Bool
algunoEsCero x y = (x == 0) || (y == 0)  -}

-- pattern matching
algunoEsCero :: Float -> Float -> Bool
algunoEsCero 0 _ = True
algunoEsCero _ 0 = True
algunoEsCero _ _ = False

{- ambosSonCero :: Float -> Float -> Bool
ambosSonCero x y = (x == 0) && (y == 0) -}

-- pattern matching
ambosSonCero :: Float -> Float -> Bool
ambosSonCero 0 0 = True
ambosSonCero x y = False

{- esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | otherwise = False -}

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = digitoUnidades(div x 10)