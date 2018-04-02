{-|
Module      : Exercício 13
Description : Criador de lista de integers a partir de uma string formada por integers
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

import Data.Char
-- | ord function converts a character to its integer (ordinal) representation

converter :: [Char] -> [Int]
converter xs = conv n xs
    where
        n = length xs
        conv 0 xs = []
        conv n [] = []
        conv n (x:xs) = (digitToInt x) : (conv (n-1) xs)


main :: IO()
main = do
    print ( converter "0123456789" )
