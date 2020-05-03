module Recursion
where
    
    {- | tribonacci.-}
    tribonacci :: Int -> Int
    tribonacci n | n <= 2 = n
                 | otherwise = tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)

    {- | esMultiploDe3· 
        determina si un número es múltiplo de 3.-}
    esMultiploDe3 :: Int -> Bool
    esMultiploDe3 0 = True
    esMultiploDe3 n | n < 0 = False
                    | otherwise = esMultiploDe3 (n - 3)

    {- | diabolico
         determina si todos los dígitos de un número son 6.-}
    diabolico :: Int -> Bool
    diabolico n | n == 6 = True
                | otherwise = (mod n 10 == mod (div n 10) 10) && diabolico (div n 10)

    {- | todosDigitosIguales
         determina si todos los dígitos de un número son iguales.-}
    todosDigitosIguales :: Int -> Bool
    todosDigitosIguales n | n < 10 = True
                          | otherwise = (mod n 10 == mod (div n 10) 10) && todosDigitosIguales (div n 10)

     esPotenciaDe :: Int -> Int -> Bool
     esPotenciaDe n m = n == 1 || esPotenciaDe (div n m) m