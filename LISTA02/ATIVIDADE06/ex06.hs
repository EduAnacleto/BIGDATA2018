{-|
Module      : Lista 2 - Exercício 6
Description : Calcular a persistência aditiva de um número
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

-- | Função que computa a persistência aditiva de um número
addPersist :: Int -> Int
addPersist x = addP 0 x
    where
        addP sum numero
            | length (show numero) == 1 = sum
            | otherwise            = addP (sum + 1) ( sumDigt numero )


main :: IO()
main = do
    print ( addPersist 999999 )
    print ( addPersist 9999999 )
    print ( addPersist 99999999 )
    print ( addPersist 999999999 )
    print ( addPersist 9999999999 )
    print ( addPersist 99999999999 )
