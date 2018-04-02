{-|
Module      : Exercício 2
Description : Verificação se a entrada é multiplo de três
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}

module Main where


-- | Função que retorna verdadeiro se a entrada for múltiplo de três e falso caso contrário
mult3 :: Integer -> Bool
mult3 x
    | x `mod` 3 == 0 = True
    | otherwise      = False

main :: IO()
main = do
    print ( mult3 12 )
