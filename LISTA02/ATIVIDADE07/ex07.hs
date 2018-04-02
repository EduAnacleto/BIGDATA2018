{-|
Module      : Lista 2 - Exercício 7
Description : Calcular o coeficiente binomial de (n,m)
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

-- | Função que calcula o coeficiente binomial
bin :: Integer -> Integer -> Integer
bin n m
    | m > n            = error "O primeiro valor deve ser maior ou igual ao segundo valor."
    | m < 0            = error "O segundo valor deve ser maior ou igual a do que zero."
    | m < (n `quot` 2) = binAux n m
    | otherwise        = binAux n (n-m)
    where 
        binAux n m = (fatorial 1 n m) `quot` (fatorial 1 m m)
        fatorial prod _ 0 = prod
        fatorial prod a b = fatorial (prod * a) (a-1) (b-1)


main :: IO()
main = do
    print ( bin 13 6 )
