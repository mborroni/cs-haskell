f x y = x * x + y * y

g x y z = x + y + z * z

doble x = 2 * x

suma x y = x + y

normaVectorial x1 x2 = sqrt (x1^2 + x2^2)

funcionConstante8 x = 8

-- case functions

f1 n | n == 0 = 1
     | othn /= 0 = 0

f2 n | n == 0 = 1
     | otherwise 0

-- minor/major

signo1 n | n > 0 = 1
         | n == 0 = 0
         | n < 0 = -1

signo2 n | n > 0 = 1
         | n == 0 = 0
         | otherwise = -1

maximo x y | x >= y = x
           | otherwise = y

-- random functions

f1 n | n >= 3 = 5

f2 n | n >= 3 = 5
     | n <= 1 = 8

f3 n | n >= 3 = 5
     | n == 2 = undefined
     | otherwise = 8

-- wrong conditions declaration

f4 n | n >= 3 = 5
     | n <= 9 = 7

f5 n | n <= 9 = 7
     | n >= 3 = 5

-- pattern machine

{- f n | n == 0 = 1
    | n /= 0 = 0 -}

f 0 = 1
f n = 0

{- signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1 -}

signo 0 = 0
signo n | n > 0 = 1
        | otherwise = -1

-- quadratic formula

{- cantidadDeSoluciones b c | b ^2 - 4* c > 0 = 2
                         | b ^2 - 4* c == 0 = 1
                         | otherwise = 0 -}

cantidadDeSoluciones b c | d > 0 = 2
                         | d == 0 = 1
                         | otherwise = 0
                           where d = b ^2 - 4* c

-- type declaration
maxInt :: Int -> Int -> Int 
maxInt x y | x >= y = x
           | otherwise = y

maxRac :: Float -> Float -> Float
maxRac x y | x >= y = x
              | otherwise = y

mayorA9 :: Int -> Bool
mayorA9 n | n > 9 = True
          | otherwise = False

{- esPar :: Int -> Bool
esPar n | mod n 2 == 0 = True
        | otherwise = False -}

esPar :: Int -> Bool
esPar n = mod n 2 == 0

esImpar :: Int -> Bool
esImpar n = not (esPar n)