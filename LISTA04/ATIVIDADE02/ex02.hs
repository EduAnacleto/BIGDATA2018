{-|
Module      : Lista 4 - Exercício 2
Description : Função que calcula a soma da diagonal principal de uma matriz
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

-- | Função que pega o valor de uma posição do vetor
getVectorValue :: Integer -> [Double] -> Double
getVectorValue p x = getVV 1 x
    where
        getVV k (x:xs)
            | k == p     = x
            | otherwise  = getVV (k+1) xs

-- | Função que soma a diagonal principal da matriz
sumDiagMatrix :: [[Double]] -> Double
sumDiagMatrix mX = sumDM 0 1 mX
    where
        sumDM :: Double -> Integer -> [[Double]] -> Double
        sumDM sum k []       = sum
        sumDM sum k (mX:mXS) = sumDM (sum + (getVectorValue k mX)) (k+1) mXS


main :: IO()
main = do
    print ( sumDiagMatrix [[1,1,1,2],[2,2,2,2],[3,3,3,2],[9,9,9,9]] )
