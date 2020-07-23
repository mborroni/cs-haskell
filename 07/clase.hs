{- 
vacio :: [Int]
vacio = []

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys

agregar :: Int -> [Int] -> [Int]
agregar x c | pertenece x c = c
            | otherwise = x : c -}

{- implementando Set -}
type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys

agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c
            | otherwise = x : c

incluido :: Set Int -> Set Int -> Bool
incluido [] _ = True
incluido (x:xs) c = pertenece x c && incluido xs c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

union :: Set Int -> Set Int -> Set Int
union [] c = c
union (x:xs) c | not (pertenece x c) = union xs (agregar x c) 
               | otherwise = union xs c

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] c = c
interseccion (x:xs) c | not (pertenece x c) = interseccion xs (agregar x c)
                      | otherwise = interseccion xs c

quitar :: Int -> Set Int -> Set Int
quitar n [] = []
quitar n (x:xs) | x == n = xs
                | otherwise = x : quitar n xs

diferencia :: Set Int -> Set Int -> Set Int
diferencia c [] = c
diferencia c (x:xs) | pertenece x c =  diferencia (quitar x c) xs
                    | otherwise = diferencia c xs

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica c1 c2 = union (diferencia c1 c2) (diferencia c2 c1)

{- perteneceConjuntos :: Set Int -> Set (Set Int) -> Bool
perteneceConjuntos xs [] = False
perteneceConjuntos xs (ys:yss) = iguales xs ys || perteneceConjuntos xs yss

agregarConjuntos :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarConjuntos xs xss | perteneceConjuntos xs xss = xss
                        | otherwise = xs:xss

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = [] 
agregarATodos x (c:cs) = agregarConjuntos (agregar x c) (agregarATodos x cs)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = union (partes xs) (agregarATodos x (partes xs)) -}