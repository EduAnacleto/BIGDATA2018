{-|
Module      : Lista 3 - Exercício 7
Description : Função que retorna o tamanho da lista formada pela aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.
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
collatzLen x = colSum 0 x
    where
        colSum sum 1 = sum
        colSum sum x = colSum (sum+1) (collatz x)


main :: IO()
main = do
    print ( collatzLen 3 )
