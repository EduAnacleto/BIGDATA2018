{-|
Module      : Lista 3 - Exercício 6
Description : Função collatz x que retorna x/2, se x for par e (3x+1) se x for impar.
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


main :: IO()
main = do
    print ( collatz 101 )
