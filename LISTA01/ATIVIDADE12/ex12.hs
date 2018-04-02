{-|
Module      : Exercício 12
Description : Concatenador de strings que concatena duas strings separando por um espaço
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}

module Main where

concatenar :: String -> String -> String
concatenar a b = a ++ " " ++ b


main :: IO()
main = do
    print ( concatenar "valor" "numero" )
