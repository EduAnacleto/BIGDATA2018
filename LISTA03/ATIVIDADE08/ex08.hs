{-|
Module      : Lista 3 - Exercício 8
Description : Encontra o número x entre 1 (um) e 1.000.000 que tem a maior sequência de Coolatz
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

-- | Função collatz
collatz :: Integer -> Integer
collatz x
    | x `mod` 2 == 0 = x `quot` 2
    | otherwise      = 3 * x + 1


-- | Função collatzLen
collatzLen :: Integer -> Integer
collatzLen x = colSum 0 x x
    where
        colSum :: Integer -> Integer -> Integer -> Integer
        colSum sum 1 _   = sum
        colSum sum x num = colSum (sum+1) (collatz x) num

-- | 
pegarXMC :: Integer -> Integer
pegarXMC m 
    | m < 1     = error "Informe um numero maior do que zero."
    | m == 1    = 1
    | otherwise = getMC 1 0 0 0 
    where
        getMC xMax i quant max
            | i == m + 1   = xMax
            | quant > max  = getMC     i (i+1) (collatzLen (i+1)) quant
            | otherwise    = getMC  xMax (i+1) (collatzLen (i+1))   max
            


lista :: [Integer] -> Integer -> Integer -> [Integer]
lista xs x q 
     | x == q + 1  = xs
     | otherwise = lista (xs ++ [collatzLen x ]) (x+1) q
 

main :: IO()
main = do
    print ( pegarXMC 1000000 )
