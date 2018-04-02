{-|
Module      : Lista 2 - Exercício 3
Description : Multiplicação etíope entre dois números
Copyright   : (c) Eduardo Anacleto, 2018
License     : GPL-3
Maintainer  : eduardo.anacleto@gmail.com

-}


module Main where


-- | Multiplicação Etíope entre dois números
multEtiope :: Integer -> Integer -> Integer
multEtiope x y = multE x y 0
    where 
        multE 1 y sum = sum + y
        multE x y sum
            | (x `mod` 2 == 0) && (y `mod` 2 == 0) = multE (x `quot` 2) (y*2) sum
            | otherwise                            = multE (x `quot` 2) (y*2) (sum + y)
            

main :: IO()
main = do
    print ( multEtiope 478 758 )

