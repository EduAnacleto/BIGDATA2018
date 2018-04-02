{-|
Module      : Lista 3 - Exercício 1
Description : Determinar se um número inteiro é divisível pelos números detre um e 20.
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where 

divisivel20 :: Integer -> Bool
divisivel20 x = divRec x 20 
    where
        divRec :: Integer -> Integer -> Bool
        divRec x 1 = True
        divRec x q
            | x `mod` q /= 0 = False
            | otherwise      = divRec x (q-1)



main :: IO()
main = do
    print (  divisivel20 232792560 )
    print (  divisivel20 465585120 )
    print (  divisivel20 698377680 )
    print (  divisivel20 931170240 )
    print (  divisivel20 931170241 )
    
