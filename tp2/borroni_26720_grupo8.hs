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

{- | Ejercicio 1. sonCoprimos dados dos números naturales decide si son coprimos. -}
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = mcd n m == 1

{- miramos máximo común divisor entre a y m = d 
si d no divide a b ya sé que mi ecuación no tiene solución
si lo divide entonces mi ecuación es igual a a/d * X ≅ b/d (mod m/d) -}
ecEquivalente :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
ecEquivalente (a, b, m) | mod b d /= 0 = undefined
                        | otherwise = (div a d, div b d, div m d)
                        where d = mcd a m

{- prop adicional de que a y m eran coprimos 
    resto de s * b en la división por m, modulo m, 
    donde s es un número que verifica que d = s * a + t * m
    emcd = extendido máximo común divisor -}
solucionEcConPropAdic :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEcConPropAdic (a, b, m) = (mod (s * b) m, m)
                                where (d, s, t) = emcd a m 

solucionEc :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

obtenerExponenteDeCifradoDesde :: Integer -> Integer -> Integer
obtenerExponenteDeCifradoDesde e m | (e > 2 && e <= (m - 2)) && sonCoprimos e m = e
                                   | otherwise = obtenerExponenteDeCifradoDesde (e + 1) m

obtenerExponenteDeCifrado :: Integer -> Integer
obtenerExponenteDeCifrado m =  obtenerExponenteDeCifradoDesde 2 m

obtenerExponenteDeDescifrado :: Integer -> Integer -> Integer
obtenerExponenteDeDescifrado e m = mod x y
                    where (x, y) = solucionEc (e, 1, m)

generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
generarClaves p q | n > 127 = ((n, d), (n, e))
                  | otherwise = undefined
    where n = p * q
          m = (p - 1) * (q - 1)
          e = obtenerExponenteDeCifrado m
          d = obtenerExponenteDeDescifrado e m

obtenerASCII :: String -> [Integer]
obtenerASCII [] = []
obtenerASCII (x:xs) = (fromIntegral (ord x)):obtenerASCII xs

encriptarNumeros :: (Integer, Integer) -> [Integer] -> [Integer]
encriptarNumeros _ [] = []
encriptarNumeros (n, d) (a:as) = numeroEncriptado : encriptarNumeros (n, d) as
    where numeroEncriptado = mod (a^d) n

encriptar :: (Integer, Integer) -> String -> [Integer]
encriptar clave mensaje = encriptarNumeros clave mensajeASCII
    where mensajeASCII = obtenerASCII mensaje

obtenerMensaje :: [Integer] -> String
obtenerMensaje [] = []
obtenerMensaje (x:xs) = chr (fromInteger x) : obtenerMensaje xs

desencriptar :: (Integer, Integer) -> [Integer] -> String
desencriptar clave mensaje = obtenerMensaje mensajeDesencriptado
    where mensajeDesencriptado = desencriptarNumeros clave mensaje

desencriptarNumeros :: (Integer, Integer) -> [Integer] -> [Integer]
desencriptarNumeros _ [] = []
desencriptarNumeros (n, e) (b:bs) = numeroDesencriptado : desencriptarNumeros (n, e) bs
    where numeroDesencriptado = mod (b^e) n

