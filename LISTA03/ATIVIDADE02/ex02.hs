{-|
Module      : Lista 3 - Exercício 2
Description : Função que retorna true para o primeiro número natual que é divisível pelos número dentre um e 20.
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

-- | Lista com os números inteiros de 1 até 20
lista20 :: [Integer]
lista20 = [ x | x <- [1..20] ]

-- | Função para pegar o primeiro número inteiro diferente de k em uma lista de números inteiros positivos
getFD :: Integer -> [Integer] -> Integer
getFD k [] = 0
getFD k (x:xs)
    | x == k    = getFD k xs
    | otherwise = x

-- | Função que mapeia uma lista dividindo por k os números que são divisíveis por k
mapDiv :: Integer -> [Integer] -> [Integer]
mapDiv k x = mapDiv' k x []
    where
        mapDiv' _ [] y = y
        mapDiv' k (x:xs) y
            | x `mod` k == 0 = mapDiv' k xs (y ++ [x `div` k])
            | otherwise      = mapDiv' k xs (y ++ [x])

-- | Função que retorna o máximo divisor comum de uma lista de números inteiros
mdc :: [Integer] -> Integer
mdc x = mdc' 1 1 x
    where
        mdc' p 0 _  = p
        mdc' p c x  = mdc' (p*c) nextC (mapDiv nextC x)
            where
                nextC = getFD 1 x


main :: IO()
main = do
    print ( mdc lista20 )

    
