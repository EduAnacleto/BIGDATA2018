{-|
Module      : Lista 3 - Exercício 4
Description : Soma dos números de Fibonacci pares não maiores que 4.000.000
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
    | k < 1     = error "Informe um número maior ou igual a do que 1 (um)."
    | otherwise = fibNext (k-2) 1 1 [1,1]
    where 
        fibNext 0 p1 p2 xs = xs
        fibNext k p1 p2 xs = fibNext (k-1) p2 (p1+p2) (xs ++ [p1+p2])


sumFibPar :: Integer -> Integer
sumFibPar n = sumFP n 0 (fibList n)
    where
        sumFP 0 sum _ = sum
        sumFP 1 sum _ = sum
        sumFP 2 sum _ = sum
        sumFP k sum (x:y:z:xs) = sumFP (k-3) (sum+z) xs


main :: IO()
main = do
    print ( sumFibPar 33 )
