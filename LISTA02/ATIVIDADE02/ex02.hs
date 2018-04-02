{-|
Module      : Lista 2 - Exercício 2
Description : Determinar o tipo do triângulo formado pelos três lados x, y e z
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

classTriangulo :: Integer -> Integer -> Integer -> [Char]
classTriangulo a b c
    | ehTriangulo a b c == False = "Nao eh triangulo."
    | (a == b) && (a == c) && (b == c)  = "Triangulo equilatero."
    | (a /= b) && (a /= c) && (b /= c)  = "Triangulo escaleno."
    | otherwise = "Triangulo isoceles."


main :: IO()
main = do
    print ( classTriangulo 10 10 10 )
