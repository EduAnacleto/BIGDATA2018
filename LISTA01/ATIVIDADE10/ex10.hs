{-|
Module      : Exercício 10
Description : Lista com os 10 últimos anos bissextos
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


takeT :: Int -> Int -> [a] -> [a]
takeT l 0 xs     = []
takeT l n []     = []
takeT l n (x:xs) 
    | l > n  =  takeT (l-1)  n  xs 
    | otherwise  =  x : ( takeT (l-1) (n-1) xs )
    
takeTail :: Int -> [a] -> [a]
takeTail k lista 
    | l < k = error "Quantidade de elementos a serem buscados maior do que o tamanho da lista."
    | otherwise = takeT l k lista
    where
        l = length lista



main :: IO()
main = do
    print ( takeTail 10 lista )
