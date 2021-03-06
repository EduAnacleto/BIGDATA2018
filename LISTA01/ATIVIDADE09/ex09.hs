{-|
Module      : Exercício 9
Description : Lista com os 10 primeiros anos bissextos
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

bissexto :: Integer -> Bool
bissexto ano
    | isDiv 400                  = True
    | isDiv 4 && not( isDiv 100) = True
    | otherwise                  = False
    where 
        isDiv :: Integer -> Bool
        isDiv n = ano `rem` n == 0

lista = [ x | x <-[1..2018], bissexto x ]


main :: IO()
main = do
    print ( take 10 lista )
