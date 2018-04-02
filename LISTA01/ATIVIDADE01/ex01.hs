{-|
Module      : Exercício 1
Description : Aplicação de operações básicas (multiplicação e soma)
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}

module Main where

-- | Módulo principal
main :: IO()
main = do
	print ( 2 * 3 + 5 ) 
	print ( 2 + 2 * 3 + 1 ) 
	print ( 3^4 + 5 * 2^5 + 1 ) 
