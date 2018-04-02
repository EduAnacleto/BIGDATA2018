{-|
Module      : Lista 4 - Exercício 1
Description : Função que gera uma matriz identidade de tamanho n
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where


-- | vetor aux identidade
vetorAI :: Integer -> Integer -> [Integer]
vetorAI k n = [ 0 | x <-[1, 2.. (k-1)]] ++ [1] ++ [ 0 | x <-[1, 2.. (n-k)]]


-- | Gerar matriz identidade
matrizIdent :: Integer -> [[Integer]]
matrizIdent n 
    | n < 1 = error "a dimensão da matriz deve ser maior do que zero"
    | otherwise = matrizI 1 []
    where
        matrizI k mX 
            | k == n     = mX ++ [vetorAI n n]
            | otherwise  = matrizI (k+1) (mX ++ [vetorAI k n])
 


main :: IO()
main = do
    print ( matrizIdent 100 )
