module Vectores(estanRelacionados, prodInterno, todoMenor, distanciaPuntos, posicPrimerPar, crearPar, invertir)
where
    
    claseEquivalencia :: Float -> Int
    claseEquivalencia x | x <= 3 = 1
                        | x > 3 && x <= 7 = 2
                        | otherwise = 3

    {- | estanRelacionados
        dados dos números reales, decide si están relacionados considerando
        la relación de equivalencia en R cuyas clases de equivalencia son:
        (−∞, 3], (3, 7] y (7, ∞).-}
    estanRelacionados :: Float -> Float -> Bool
    estanRelacionados x y = claseEquivalencia x == claseEquivalencia y

    {- | prodInterno
         calcula el producto interno entre dos vectores de R2.-}
    prodInterno :: (Float, Float) -> (Float, Float) -> Float
    prodInterno (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2) 

    esMenor :: Float -> Float -> Bool
    esMenor x y = x < y

    {- | todoMenor
        dados dos vectores de R2, decide si es cierto que cada coordenada del primer
        vector es menor a la coordenada correspondiente del segundo vector.-}
    todoMenor :: (Float, Float) -> (Float, Float) -> Bool
    todoMenor (x1, y1) (x2, y2) = esMenor x1 x2 && esMenor y1 y2

     {- | distanciaPuntos
        calcula la distancia entre dos puntos de R2.-}
    distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
    distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

    esPar :: Int -> Bool
    esPar x = mod x 2 == 0

    {- | posicPrimerPar 
        dada una terna de enteros, devuelve la posición del primer número par si
        es que hay alguno, y devuelve 4 si son todos impares.-}
    posicPrimerPar :: (Int, Int, Int) -> Int
    posicPrimerPar (x, y, z) | esPar x = 0
                             | esPar y = 1
                             | esPar z = 2
                             | otherwise = 4

    {- | crearPar 
    a -> b -> (a, b): crea un par a partir de sus dos componentes dadas por
    separado (debe funcionar para elementos de cualquier tipo.-}
    crearPar :: v -> w -> (v, w)
    crearPar x y = (x, y)

    {- | invertir
    (a, b) -> (b, a): invierte los elementos del par pasado como parámetro
    (debe funcionar para elementos de cualquier tipo.-}
    invertir :: (v, w) -> (w, v)
    invertir (x, y) = (y ,x) 