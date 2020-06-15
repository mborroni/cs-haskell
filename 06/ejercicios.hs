module Listas
where

    {-| productoria :: [Int] -> Int que devuelve la productoria de los elementos.-}
    productoria :: [Int] -> Int
    productoria [] = 1
    productoria (x:xs) = x * productoria xs 

    {-| sumarN :: Int -> [Int] -> [Int] que dado un número N y una lista xs, suma N a cada
    elemento de xs.-}
    sumarN :: Int -> [Int] -> [Int]
    sumarN _ [] = []
    sumarN n (x:xs) = (x + n) : (sumarN n xs)

    {-| sumarElPrimero :: [Int] -> [Int] que dada una lista no vacía xs, suma el primer
    elemento a cada elemento de xs. Ejemplo sumarElPrimero [1,2,3] -> [2,3,4].-}
    sumarElPrimero :: [Int] -> [Int]
    sumarElPrimero [] = []
    sumarElPrimero (x:xs) = x + x : sumarN x xs

    {-| ultimoElemento :: [Int] -> Int devuelve el último elemento de una lista.-}
    ultimoElemento :: [Int] -> Int
    ultimoElemento (x:xs) | xs == [] = x
                          | otherwise = ultimoElemento xs

    {-|sumarElUltimo :: [Int] -> [Int] que dada una lista no vacía xs, suma el último
    elemento a cada elemento de xs. Ejemplo sumarElUltimo [1,2,3] -> [4,5,6].-}
    sumarElUltimo :: [Int] -> [Int]
    sumarElUltimo [] = []
    sumarElUltimo (x:xs) = x + (ultimoElemento xs) : sumarN (ultimoElemento xs) xs

    {-| esPar :: Int -> Bool verifica que un número sea par.-}
    esPar :: Int -> Bool
    esPar n = mod n 2 == 0

    {-| pares :: [Int] -> [Int] que devuelve una lista con los elementos pares de la lista
    original. Ejemplo pares [1,2,3,5,8] -> [2,8].-}
    pares :: [Int] -> [Int]
    pares [] = []
    pares (x:xs) | esPar x = x : pares xs
                 | otherwise = pares xs

    {-| quitar :: Int -> [Int] -> [Int] que elimina la primer aparición del elemento en la lista.-}
    quitar :: Int -> [Int] -> [Int]
    quitar n [] = []
    quitar n (x:xs) | x == n = xs
                    | otherwise = x : quitar n xs

    {-| quitarTodas :: Int -> [Int] -> [Int] que elimina todas las apariciones del elemento en la lista.-}
    quitarTodas :: Int -> [Int] -> [Int]
    quitarTodas _ [] = []
    quitarTodas n (x:xs) | x == n = quitarTodas n xs
    quitarTodas n (x:xs) | otherwise = x : quitarTodas n xs

    {-| pertenece :: Int -> [Int] -> Bool indica si un elemento aparece en la lista.-}
    pertenece :: Int -> [Int] -> Bool
    pertenece _ [] = False
    pertenece n (x:xs) = x == n || pertenece n xs 

    {-| hayRepetidos :: [Int] -> Bool que indica si una lista tiene elementos repetidos.-}
    hayRepetidos :: [Int] -> Bool
    hayRepetidos [] = False
    hayRepetidos (x:xs) | pertenece x xs = True
                        | otherwise = hayRepetidos xs

    {-| eliminarRepetidosAlFinal :: [Int] -> [Int] que deja en la lista la primera aparición
    de cada elemento, eliminando las repeticiones adicionales.-}

    {-| eliminarRepetidosAlInicio :: [Int] -> [Int] que deja en la lista la última aparición
    de cada elemento, eliminando las repeticiones adicionales.-}

    esMayor :: Int -> [Int] -> Bool
    esMayor _ [] = True
    esMayor n (x:xs) | n < x = False
                     | otherwise = esMayor n xs

    {-| maximo :: [Int] -> Int que calcula el máximo elemento de una lista no vacía.-}
    maximo :: [Int] -> Int
    maximo [] = 0
    maximo (x:xs) | esMayor x xs = x
                  | otherwise = maximo xs 

    {-| ordenar :: [Int] -> [Int] que ordena los elementos de forma creciente.-}
    ordenar :: [Int] -> [Int]
    ordenar [] = []
    ordenar l = ordenar (quitar (maximo l) l) ++ (maximo l : [])

    {- otra forma de hacer ordenar es tener una función que busque el menor y concatene a partir del menor.
    esMenor :: Int -> [Int] -> Bool
    esMenor _ [] = False
    esMenor n (x:xs) | n < x && xs == [] = True
                     | otherwise = esMenor n xs 

    minimo :: [Int] -> Int
    minimo [] = 0
    minimo (x:xs) | esMenor x xs = x
                  | otherwise = minimo xs-}
    
    {-| quitarUltimo :: [Int] -> [Int] dada una lista, la devuelve sin el último elemento.-}
    quitarUltimo :: [Int] -> [Int]
    quitarUltimo [] = []
    quitarUltimo (x:xs) | xs == [] = []
                        | otherwise = x : quitarUltimo xs

    {-| reverso :: [Int] -> [Int] que dada una lista invierte su orden.-}
    reverso :: [Int] -> [Int]
    reverso [x] = [x]
    reverso l = ultimoElemento l : reverso (quitarUltimo l)

    {-| concatenar :: [Int] -> [Int] -> [Int] que devuelve la concatenación de la primera
    lista con la segunda. Ejemplo concatenar [1,2,3] [4,5,6] -> [1,2,3,4,5,6],
    concatenar [] [4,5,6] -> [4,5,6]. Esta operación está en el prelude y se escribe como (++).-}
    concatenar :: [Int] -> [Int] -> [Int]
    concatenar [] l2 = l2
    concatenar l1 l2 = concatenar (quitarUltimo (l1)) ((ultimoElemento (l1)) : l2)

    {-| zipi :: [a] -> [b] -> [(a,b)] que devuelve una lista de tuplas, cada tupla contiene
    elementos de ambas listas que ocurren en la misma posición. En caso que tengan distintas
    longitudes, la longitud de la lista resultado es igual a la longitud de la lista más chica
    pasada por parámetro. Ejemplo zipi [1,2,3] ['a','b','c']
    [(1,'a'), (2,'b'), (3,'c')], zipi [1,2,3] ['a','b'] [(1,'a'), (2,'b')]. Esta
    operación está en el prelude y se escribe como zip.-}
    zipi :: [a] -> [b] -> [(a,b)]
    zipi [] _ = []
    zipi _ [] = []
    zipi (x:xs) (y:ys) = (x, y) : zipi xs ys