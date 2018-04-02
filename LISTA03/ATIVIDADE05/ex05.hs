{-|
Module      : Lista 3 - Exercício 5
Description : Produto escalar entre dois vetores
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

-- | Norma de um vetor
normaVetor :: [Double] -> Double
normaVetor x = sqrt (sumProd 0 x)
    where 
        sumProd sum [] = sum
        sumProd sum (x:xs) = sumProd (sum+x) xs


-- | Produto escalar 
escalarProd :: [Double] -> [Double] -> Double -> Double
escalarProd x y teta 
    | (length x) /= (length y) = error "As dimensões dos vetores devem ser iguais."
    | otherwise = (normaVetor x) * (normaVetor y) * (cos teta) 


main :: IO()
main = do
    print ( escalarProd [7.1, 1.1, 0.1, 20.1] [1, 1, 1, 1] 0 )
