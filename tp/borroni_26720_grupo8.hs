mayorComunDivisor :: Integer -> Integer -> Integer
mayorComunDivisor n m | m == 0 = n
                      | otherwise = mayorComunDivisor m (mod n m)

{- | Ejercicio 1. sonCoprimos dados dos números naturales decide si son coprimos. -}
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = mayorComunDivisor n m == 1

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = menorDivisorDesde n (k + 1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

{- | Función auxiliar. esPrimo dado un número natural decide si es primo. -}
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

{- | Función auxiliar. esAPseudoPrimo dados dos números naturales decide si un número es n-pseudoprimo. -}
esAPseudoPrimo :: Integer -> Integer -> Bool 
esAPseudoPrimo a n | n < 3 = False 
                   | otherwise = not (esPrimo n) && (mod (a^(n-1) - 1) n) == 0

{- | Ejercicio 2. es2PseudoPrimo dados un número natural decide si un número es 2-pseudoprimo. -}
es2PseudoPrimo :: Integer -> Bool
es2PseudoPrimo n = esAPseudoPrimo 2 n

{- | Función auxiliar. es3PseudoPrimo dados un número natural decide si un número es 3-pseudoprimo. -}
es3PseudoPrimo :: Integer -> Bool
es3PseudoPrimo n = esAPseudoPrimo 3 n

{- | Ejercicio 3. cantidad3PseudoPrimos dado un número natural m calcula la cantidad de 3-pseudoprimos que hay entre 1 y m inclusive. 
    Se fija si el número cumple la condición de ser 3-pseudoprimo, si es así, suma 1 y ejecuta una recursión, sino solamente ejecuta el paso recursivo. 
    La función no está definida para el intervalo (-∞,1) aunque se podría haber definido cambiando la primer sentencia por cantidad3PseudoPrimos m | m <= 1 = 0. -}
cantidad3PseudoPrimos :: Integer -> Integer
cantidad3PseudoPrimos 1 = 0
cantidad3PseudoPrimos m  | es3PseudoPrimo m = 1 + cantidad3PseudoPrimos (m - 1)
                         | otherwise = cantidad3PseudoPrimos (m - 1)

{- | Función auxiliar. kesimo2y3PseudoPrimoDesde dados dos números un índice k y el inicio del intervalo, siendo este (n,∞), 
    devuelve el k-esimo número que es simultáneamente 2-pseudoprimo y 3-pseudoprimo. -}
kesimo2y3PseudoPrimoDesde :: Integer -> Integer -> Integer
kesimo2y3PseudoPrimoDesde k n | k == 0 = n - 1
                              | es2PseudoPrimo n && es3PseudoPrimo n = kesimo2y3PseudoPrimoDesde (k - 1) (n + 1)
                              | otherwise = kesimo2y3PseudoPrimoDesde k (n + 1)

{- | Ejercicio 4. kesimo2y3PseudoPrimo dado un número natural k calcula el k-esimo número que es simuláneamente 2-pseudoprimo y 3-pseudoprimo. -}
kesimo2y3PseudoPrimo :: Integer -> Integer
kesimo2y3PseudoPrimo k = kesimo2y3PseudoPrimoDesde k 1

{- | Función auxiliar. esCarmichaelAux se fija si un número es coprimo y n-pseudoprimo para todos los casos dentro del rango [a,n]. 
    Declaro el inicio del intervalo a para poder guardar una referencia para definir el caso base.-}
esCarmichaelAux :: Integer -> Integer -> Bool
esCarmichaelAux a n | a == n - 1 && esAPseudoPrimo a n = True
                    | sonCoprimos a n && not (esAPseudoPrimo a n) = False
                    | otherwise = esCarmichaelAux (a + 1) n

{- | Ejercicio 5. esCarmichael dado un número natural decide si es un número de Carmichael. -}
esCarmichael :: Integer -> Bool
esCarmichael n = esCarmichaelAux 1 n