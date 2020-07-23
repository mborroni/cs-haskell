module FuncionesAuxiliares
where

    -- esFibonacci
    fibo :: Int -> Int
    fibo n | n <= 1 = 1
           | otherwise = fibo (n - 1) + fibo (n - 2)

    fiboHasta :: Int -> Int -> Bool 
    fiboHasta n i | fibo i == n = True
                  | fibo i > n = False
                  | otherwise = fiboHasta n (i + 1)

    {- | Implementar esFibonacci :: Int -> Bool que dado un número entero n ≥ 0 decide si n
       es un número de Fobonacci.-}
    esFibo :: Int -> Bool
    esFibo n = fiboHasta n 1


    -- esSumaInicialDePrimos 
    proximoPrimo :: Int -> Int
    proximoPrimo n | esPrimo next = next
                   | otherwise = proximoPrimo next
                    where next = n + 1

    menorDivisorDesde :: Int -> Int -> Int
    menorDivisorDesde n k | mod n k == 0 = k
                          | otherwise = menorDivisorDesde n (k + 1)

    menorDivisor :: Int -> Int
    menorDivisor n = menorDivisorDesde n 2

    esPrimo :: Int -> Bool
    esPrimo 1 = False
    esPrimo n = menorDivisor n == n

    sumaPrimos :: Int -> Int 
    sumaPrimos 0 = 0
    sumaPrimos n | esPrimo n = n + sumaPrimos(n - 1)
                 | otherwise = sumaPrimos (n - 1)

    sumaPrimosDesde :: Int -> Int -> Int
    sumaPrimosDesde n m | sumaPrimos n > m = 0
                        | sumaPrimos n == m = m
                        | otherwise = sumaPrimosDesde (n + 1) m

    {- | Implementar esSumaInicialDePrimos :: Int -> Bool que dado un número entero n ≥ 0
       decide si n es igual a la suma de los m primeros números primos, para algún m.-}
    esSumaInicialDePrimos :: Int -> Bool
    esSumaInicialDePrimos n = n == sumaPrimosDesde 1 n 

    -- esSumaDeDosPrimos
    esSumaDeDosPrimosDesde :: Int -> Int -> Bool
    esSumaDeDosPrimosDesde n m | n < 1 = False
                               | esPrimo n && esPrimo (m - n) = True
                               | otherwise = esSumaDeDosPrimosDesde (n - 1) m

    {- | Implementar esSumaDeDosPrimos :: Int -> Bool que, dado un número natural n,
    determine si puede escribirse como suma de dos números primos. -}
    esSumaDeDosPrimos :: Int -> Bool 
    esSumaDeDosPrimos n = esSumaDeDosPrimosDesde n n

    -- goldbach
    esPar :: Int -> Bool
    esPar n = mod n 2 == 0

    {- |  Conjetura de Christian Goldbach, 1742: todo número par mayor que 2 puede escribirse
    como suma de dos números primos. Escribir una función que pruebe la conjetura hasta un
    cierto punto. goldbach :: Int -> Bool -}
    goldbach :: Int -> Bool
    goldbach n | n < 2 = False
               | n == 2 = True
               | otherwise = esPar n && goldbach (n - 2)

    