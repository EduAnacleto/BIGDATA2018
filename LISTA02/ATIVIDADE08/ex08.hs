{-|
Module      : Lista 2 - Exercício 8
Description : Calcular o elemento (i,j) do triângulo de pascal
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

-- | Função que calcula o elemento (i,j) do triângulo de pasqual
bin :: Integer -> Integer -> Integer
bin i j
    | j > i            = error "O primeiro valor deve ser maior ou igual ao segundo valor."
    | j < 0            = error "O segundo valor deve ser maior ou igual a do que zero."
    | j < (i `quot` 2) = binAux i j
    | otherwise        = binAux i (i-j)
    where 
        binAux i j = (fatorial 1 i j) `quot` (fatorial 1 j j)
        fatorial prod _ 0 = prod
        fatorial prod a b = fatorial (prod * a) (a-1) (b-1)


main :: IO()
main = do
    print ( bin 4 2 )
