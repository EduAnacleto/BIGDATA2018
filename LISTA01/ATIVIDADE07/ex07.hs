{-|
Module      : Exercício 7
Description : Função que recebe um ângulo a e retorna uma tupla ontendo o seno da metade desse ângulo utilizando a identidade
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

sinxDiv2 :: Double -> (Double, Double)
sinxDiv2 a = (x1, x2)
    where
        x2 = - x1
        x1 = sqrt (  (1 - cos a) / 2  )

main :: IO()
main = do
    print (  sinxDiv2 10  )
