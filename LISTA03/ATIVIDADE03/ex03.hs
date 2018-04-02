{-|
Module      : Lista 3 - Exercício 3
Description : Lista dos números de Fibonacci a partir de uma função geradora
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where


-- | Fibonacci sequenci
fibList :: Integer -> [Integer]
fibList 1 = [1]
fibList 2 = [1, 1]
fibList k
    | k < 1     = error "Informe um numero maior ou igual a 1 (um)."
    | otherwise = fibNext (k-2) 1 1 [1,1]
    where 
        fibNext 0 p1 p2 xs = xs
        fibNext k p1 p2 xs = fibNext (k-1) p2 (p1+p2) (xs ++ [p1+p2])


main :: IO()
main = do
    print ( fibList 33 )
