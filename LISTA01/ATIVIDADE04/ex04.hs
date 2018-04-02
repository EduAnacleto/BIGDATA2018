{-|
Module      : Exercício 4
Description : Verificação se a entrada é multiplo de três e cinco
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}

module Main where

mult35 :: Integer -> Bool
mult35 x
    | (x `mod` 3 == 0) && (x `mod` 5 == 0) = True
    | otherwise                        = False

main :: IO()
main = do
    print ( mult35 15  )
