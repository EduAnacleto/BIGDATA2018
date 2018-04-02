{-|
Module      : Lista 2 - Exercício 4
Description : Determinar se um número é primo
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where

-- | Função que determina se o número é primo
ehPrimo :: Integer -> Bool
ehPrimo x 
    | x < 2                       = False
    | x == 2                      = True   -- O número dois é primo
    | x `mod` 2 == 0              = False  -- Números pares maiores do que dois não são primos
    | xQuot2 `mod` 2 == 0         = ehP x ( xQuot2 -1 ) -- (Theorem) Não precisamos verificar os números superiores X/2
    | otherwise                   = ehP x xQuot2        
    where
        xQuot2 = x `quot` 2
        ehP x y
            | y == 1         = True
            | x `mod` y == 0 = False
            | otherwise      = ehP x (y - 2)


main :: IO()
main = do
    print ( ehPrimo             8191 )  -- Found 1456
    print ( ehPrimo             8193 )
    print ( ehPrimo           131071 )  -- Found 1460
    print ( ehPrimo           131073 )  
    print ( ehPrimo           524287 )  -- Found 1588 (Pietro Cataldi)
    print ( ehPrimo           524289 )  
    print ( ehPrimo          6700417 )  -- Found 1732 (Leonhard Euler)
    print ( ehPrimo          6700421 )  
    -- print ( ehPrimo       2147483647 )  -- Found 1772 (Leonhard Euler)
    -- print ( ehPrimo   67280421310721 )  -- Found 1855 (Thomas Clausen)
