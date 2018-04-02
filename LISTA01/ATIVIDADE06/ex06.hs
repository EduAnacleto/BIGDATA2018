{-|
Module      : Exercício 6
Description : Função que recebe um número inteiro e o divide por dois
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where


div2d :: Integer -> Double
div2d x = (fromIntegral x) / 2

main :: IO()
main = do
    print ( div2d 11 )
