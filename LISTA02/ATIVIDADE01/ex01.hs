{-|
Module      : Lista 2 - Exercício 1
Description : Determinar se três lados x, y e z podem formar um triângulo
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}

module Main where

ehTriangulo :: Integer -> Integer -> Integer -> Bool
ehTriangulo a b c
    | (x == y) == z = True
    | otherwise   = False
    where
        x = if c < b then (b-c < a) && (a < b+c) else (c-b < a) && (a < b+c)
        y = if c < a then (a-c < b) && (b < a+c) else (c-a < b) && (b < a+c)
        z = if b < a then (a-b < c) && (c < a+b) else (b-a < c) && (c < a+b)

main :: IO()
main = do
    print ( ehTriangulo 10 7 3 )
