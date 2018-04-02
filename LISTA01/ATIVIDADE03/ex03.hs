{-|
Module      : Exercício 3
Description : Verificação se a entrada é multiplo de cinco
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}

module Main where

mult5 :: Integer -> Bool
mult5 x
    | x `mod` 5 == 0 = True
    | otherwise      = False

main :: IO()
main = do
    print ( mult5 39 )
