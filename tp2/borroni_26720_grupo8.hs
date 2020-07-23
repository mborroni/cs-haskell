import Data.Char (ord, chr)
import Data.String

mcd :: Integer -> Integer -> Integer
mcd n m | m == 0 = n
        | otherwise = mcd m (mod n m)

emcd :: (Integral a) =>  a -> a -> (a, a, a)
emcd a 0 = (a, 1, 0)
emcd a b = (d, t, s - (div a b) * t)
    where
        (d,s,t) = emcd b (mod a b) 

{- | Función auxiliar. -}
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = mcd n m == 1

{- | Función auxiliar. -}
ecEquivalente :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
ecEquivalente (a, b, m) | mod b d /= 0 = undefined
                        | otherwise = (div a d, div b d, div m d)
                        where d = mcd a m

{- | Función auxiliar. -}
solucionEcConPropAdic :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEcConPropAdic (a, b, m) = (mod (s * b) m, m)
                                where (d, s, t) = emcd a m 

{- | Función auxiliar. -}
solucionEc :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

{- | Función auxiliar. Obtiene el exponente de descifrado aleatorio a partir de un número e hasta un número m.
   Obtiene el número más chico posible. -}
obtenerExponenteDeDescifradoDesde :: Integer -> Integer -> Integer
obtenerExponenteDeDescifradoDesde e m | (e > 2 && e <= (m - 2)) && sonCoprimos e m = e
                                   | otherwise = obtenerExponenteDeDescifradoDesde (e + 1) m

{- | Función auxiliar. Obtiene el exponente de descifrado aleatorio dentro del intervalo (2:m-2], 
   teniendo en cuenta la condición 2 ≤ e ≤ m − 2 y que ambos sean coprimos. -}
obtenerExponenteDeDescifrado :: Integer -> Integer
obtenerExponenteDeDescifrado m =  obtenerExponenteDeDescifradoDesde 2 m

{- | Función auxiliar. Obtiene el exponente de cifrado en base al exponente de descifdrado 
   teniendo en cuenta que d es equivalente a resolver la ecuación de congruencia eX ≡ 1 (mod m). -}
obtenerExponenteDeCifrado :: Integer -> Integer -> Integer
obtenerExponenteDeCifrado e m = mod x y
                    where (x, y) = solucionEc (e, 1, m)

{-| generarClaves. Genera una clave pública y una privada en base a dos inputs que como pre-condición tienen que ser primos diferentes.
   Si se valida que el valor de n > 127. -}
generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
generarClaves p q | n > 127 = ((n, d), (n, e))
                  | otherwise = undefined
    where n = p * q
          m = (p - 1) * (q - 1)
          e = obtenerExponenteDeDescifrado m
          d = obtenerExponenteDeCifrado e m

{- | Función auxiliar. Pasa cada cáracter del String a un array de Integer usando el número ASCII correspondiente. -}
obtenerASCII :: String -> [Integer]
obtenerASCII [] = []
obtenerASCII (x:xs) = (fromIntegral (ord x)):obtenerASCII xs

{- | Función auxiliar. Encripta el mensaje pasado a ASCII utilizando una clave pública -}
encriptarNumeros :: (Integer, Integer) -> [Integer] -> [Integer]
encriptarNumeros _ [] = []
encriptarNumeros (n, d) (a:as) = numeroEncriptado : encriptarNumeros (n, d) as
    where numeroEncriptado = mod (a^d) n

{- | encriptar. -}
encriptar :: (Integer, Integer) -> String -> [Integer]
encriptar clave mensaje = encriptarNumeros clave mensajeASCII
    where mensajeASCII = obtenerASCII mensaje

{- | Función auxiliar. Convierte un array de Integer a String convirtiendo cada número a una letra basándose en ASCII. -}
obtenerMensaje :: [Integer] -> String
obtenerMensaje [] = []
obtenerMensaje (x:xs) = chr (fromInteger x) : obtenerMensaje xs

{- | Función auxiliar. Desencripta el mensaje pasado a ASCII utilizando una clave privada -}
desencriptarNumeros :: (Integer, Integer) -> [Integer] -> [Integer]
desencriptarNumeros _ [] = []
desencriptarNumeros (n, e) (b:bs) = numeroDesencriptado : desencriptarNumeros (n, e) bs
    where numeroDesencriptado = mod (b^e) n

{- | desencriptar. -}
desencriptar :: (Integer, Integer) -> [Integer] -> String
desencriptar clave mensaje = obtenerMensaje mensajeDesencriptado
    where mensajeDesencriptado = desencriptarNumeros clave mensaje

{- Ejercicio adicional: romper código: Romper el c´odigo asociado a la clave p´ublica (100337, 60953), desencriptar la
   siguiente pregunta: [33706, 38913, 58255, 99961, 77756, 23881, 220, 77756, 1606, 38913, 77756, 78982, 18800, 91658, 91658, 58255, 77756, 96593, 58255, 438, 22839, 28700, 18800, 1606, 58255, 48389] -}

{-| Función auxiliar. -}
menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = menorDivisorDesde n (k + 1)

{-| Función auxiliar. -}
menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

{-| Función auxiliar. -}
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

{- | Función auxiliar. -}
proximoPrimo :: Int -> Int
proximoPrimo n | esPrimo next = next
               | otherwise = proximoPrimo next
               where next = n + 1

{- | Función auxiliar. Permite factorizar números a partir de un cierto número m que como pre-condición (no validada) debe ser primo. -}
factorizarDesde :: Int -> Int -> [Int]
factorizarDesde 1 _ = []
factorizarDesde n m | mod n m == 0 = m : factorizarDesde (div n m) m
                    | otherwise = factorizarDesde n (proximoPrimo m)

{- | Permite factorizar el número n de la clave pública para así encontrar p y q. -}
factorizar :: Int -> [Int]
factorizar n = factorizarDesde n 2

{- 
    Para poder calcular la clave privada lo primero que hay que hice fue factorizar el número n = 100337, que dio como resultado (269, 373) como p y q,
    indiferentemente de cuál sea p y cuál q. Con esos dos datos ya puedo calcular m = (p-1)*(q-1) = 99696.
    Con todos estos pasos realizados ya tengo la mayoría de los datos
        p = 269 q = 373
        n = 100337 m = 99696
        d = 60956 e = ?
    Me queda por calcular e que corresponde a => de ≡ 1 (mod m), resolviendo esa ecuación obtengo que e = 1001

    claves:
        - pública (100337, 60953) 
        - privada (100337, 1001)
    input:
        [33706, 38913, 58255, 99961, 77756, 23881, 220, 77756, 1606, 38913, 77756, 78982, 18800, 91658, 91658, 58255, 77756, 96593, 58255, 438, 22839, 28700, 18800, 1606, 58255, 48389]
    output:
        Cuál es tu pizza favorita? 
    respuesta:
        [22329,58255,50740,22839,96986,77756,61099,77756,50740,22839,28700,28700,22839,96986]
-}