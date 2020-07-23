module Sumatoria
where

    g1 :: Int -> Int -> Int
    g1 i n | n < i = 0
           | otherwise = g1 i (n - 1) + i^n

    aux :: Int -> Int -> Int
    aux n 0 = 0
    aux n m = m^n + aux n (m - 1)

    g2 :: Int -> Int
    g2 0 = 0
    g2 n = g2 (n - 1) + aux n n