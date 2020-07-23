fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n - 1)

prod :: Int -> Int -> Int
prod d h | d == h = d
prod d h | otherwise = h * prod d (h - 1)

prod' :: Int -> Int -> Int
prod' d h | d == h = d
prod' d h | otherwise = d * prod' (d + 1) h

esDivisor :: Int -> Int -> Bool
esDivisor n k = mod n k == 0 

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 = 1
                     | esDivisor n k = k + sumaDivisoresHasta n (k - 1)
                     | otherwise = sumaDivisoresHasta n (k - 1)

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | mod n k == 0 = k
                     | otherwise = menorDivisorDesde n (k + 1)

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

{- esPrimo' :: Int -> Bool
esPrimo' n = (n > 1) && not (tieneDivisoresDesde n 2) -}

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n - 1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                  | otherwise = minimoPrimoDesde (n + 1)

menorFactorialDesdeDesde :: Int -> Int -> Int
menorFactorialDesdeDesde m i | fact i >= m = fact i
                           | otherwise = menorFactorialDesdeDesde (i + 1) m

menorFactorialDesde :: Int -> Int
menorFactorialDesde n = menorFactorialDesdeDesde n 1