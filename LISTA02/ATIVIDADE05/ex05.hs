{-|
Module      : Lista 2 - Exercício 5
Description : Calcular a soma dos dígitos de um número
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

import Data.Char

-- | Função para somar os digitos de um número
sumDigt :: Int -> Int
sumDigt x = sumD 0 (show x)
    where
        sumD :: Int -> [Char] -> Int
        sumD sum [] = sum
        sumD sum (a:as) = sumD (sum + (digitToInt a)) as


main :: IO()
main = do
    print ( sumDigt 5555555555 )


