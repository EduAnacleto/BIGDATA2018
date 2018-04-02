{-|
Module      : Lista 3 - Exercício 8
Description : Encontra o número x entre 1 (um) e 1.000.000 que tem a maior sequência de Coolatz
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

-- | Pegar um valor em uma lista invertida
getVal :: [Integer] -> Integer -> Integer
--getVal     _  0 = error "Busca incorreta."
--getVal    []  _ = error "Informe uma lista com pelomenos um argumento."
getVal (x:xs) i
    | i == 1    = x
    | otherwise = getVal xs (i-1)


-- | Função collatz
collatz :: Integer -> Integer
collatz x
    | x `mod` 2 == 0 = x `quot` 2
    | otherwise      = 3 * x + 1


-- | Função collatzLen
collatzLen :: Integer -> [Integer] -> Integer
collatzLen x as = colSum 0 x x as
    where
        colSum :: Integer -> Integer -> Integer -> [Integer] -> Integer
        colSum sum 1 _ _ = sum
        colSum sum x num as
            | x < num = sum + (getVal as (num-x))
            | otherwise = colSum (sum+1) (collatz x) num as

{--
-- | 
pegarXMC :: Integer -> Integer
pegarXMC m 
    | m < 1     = error "Informe um numero maior do que zero."
    | m == 1    = 1
    | otherwise = getMC 0 0 0 0 []
    where
        getMC xMax i quant max as
            | i == m + 1   = xMax
            | quant > max  = getMC    i (i+1) (collatzLen (i+1) as) quant (quant : as)
            | otherwise    = getMC xMax (i+1) (collatzLen (i+1) as) max   (quant : as)
--}

collatzXMC :: Integer-> Integer -> Integer -> Integer -> Integer -> [Integer] -> Integer
collatzXMC m x q xMax qMax as
    | x == m + 1  = xMax
    | q > qMax    = collatzXMC m xNext (collatzLen xNext listaCollatz)    x    q listaCollatz
    | otherwise   = collatzXMC m xNext (collatzLen xNext listaCollatz) xMax qMax listaCollatz
    where
        xNext = x + 1
        listaCollatz = q:as

main :: IO()
main = do

    --print ( collatzLen 9 [3, 16, 8, 5, 2, 7, 1, 0] )
    print ( collatzXMC 100000 0 0 0 0 [] )

