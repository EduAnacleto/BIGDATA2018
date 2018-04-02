{-|
Module      : Exercício 5
Description : Verificação se a entrada é menor que -1 ou (maior que 1 e múltiplo de 2)
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

cond :: Integer -> Bool
cond x
    | x1 x || ( x2 x && x3 x ) = True
    | otherwise          = False
    where
        x1 x = x < -1
        x2 x = x >  1
        x3 x = x `mod` 2 == 0

main :: IO()
main = do
    print ( cond 9 )
