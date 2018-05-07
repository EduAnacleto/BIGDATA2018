{-|
Description     : Avaliação em co-agrupamento
Copyright       : (c) Eduardo Anacleto, 2018
Maintainer      : eduardo.statistic@gmail.com

Developed by    : Eduardo Anacleto
-}

module Main where

import System.IO
import System.Environment
import Text.Read
import Data.List -- (foldl1', nub, tails, intercalate, sortBy, groupBy, sort)

import Control.Parallel
import Control.Parallel.Strategies
import Data.List.Split --(chunksOf)

import qualified Data.HashMap.Strict as M

import System.Random


-- | TYPES
type ChunksOf a          = [a]

type Solution            = [([Integer], [Integer])]
type Instance            = [(Integer, [Integer])]
type FilteredInstance    = [[Integer]]
type BruteInstance       = [[Integer]]
type SolutionValue       = Integer
type Dictionary          = M.HashMap Integer Integer
type ObjectLabels        = [Integer]
type FeatureLabels       = [Integer]
type FilteredInstanceRow = [Integer]


-- | RANDOM GENERATOR FUNCTIONS
-- |##########################################################################################

-- | Linear Congruential Generator
ranNumGenerator :: Integer -> Integer -> Integer -> (Integer, Integer)
ranNumGenerator seed low high = ( toInteger seed', low + toInteger ( truncate (value_0_1 * fromIntegral (high - low) )  ) )
    where
        seedA = truncate ((fromIntegral (16807 * (seed `rem` 127773))) - ((fromIntegral seed) / 127773 ) * 2836) 
        seed' 
            | seedA < 0 = seedA + 2147483647
            | otherwise = seedA
        value_0_1 :: Double
        value_0_1 = (fromIntegral seed') / 2147483647

-- | Random List
randomList :: Integer -> Integer -> Integer -> Integer -> [Integer]
randomList seed n vMin vMax
    | n < 0 = []
    | otherwise = randomList' seed n []
    where
        randomList'    _ 0 list = list
        randomList' seed n list = randomList' seed' (n-1) ( ( randNum :: Integer) : list )
            where
                (seed', randNum) = ranNumGenerator seed vMin vMax

-- |##########################################################################################
-- |##########################################################################################




-- | BASIC FUNCTIONS 
-- | ##########################################################################################

-- |'parMap'
parmap :: NFData b => (a -> b) -> [a] -> [b]
parmap f xs = map f xs `using` parList rdeepseq


-- |'mapReduce'
mapReduce :: NFData b => (a -> b) -> (b -> b -> b) -> ChunksOf [a] -> b
mapReduce f g xs = foldl1' g $ (map f' xs `using` parList rdeepseq)
    where
        f' xi = foldl1' g $ map f xi

-- |'len' get the length of an array
len :: [a] -> Integer
len x = len' 0 x
    where
        len' n []     = n
        len' n (x:xs) = len' (n+1) xs

-- |'parLen'
parLen :: ChunksOf [a] -> Integer
parLen chunks = mapReduce (\x -> 1) (+) chunks

-- |'flapMap'
flatMap :: [[a]] -> [a]
flatMap matrix = foldl (++) [] matrix

-- |'distinctFlatMap'
distinctFlatMap :: [[Integer]] -> [Integer]
distinctFlatMap  = sort . nub . flatMap

-- |'zipWithIndexBackLen'
-- |Está função aplica o zipWithIndex e retornando também a dimensão do vetor zipado
zipWithIndexBackLen :: [a] -> (Integer, [(a, Integer)])
zipWithIndexBackLen x = zipWIBL x [1,2..] []
    where
        zipWIBL (x:[]) (y:_) h = (y, h++[(x, y)])
        zipWIBL (x:xs) (y:ys) h = zipWIBL xs ys (h++[(x, y)])

-- |'zipWithIndex'
zipWithIndex :: [a] -> [(Integer, a)]
zipWithIndex = zip [1,2..]


-- | ##########################################################################################
-- | ##########################################################################################




-- |'PREPROCESSING FUNCTIONS
-- |##########################################################################################

-- | Leitura de arquivo
-- |'parseFile' parses a space separated file to a list of lists of Integer
parseFile :: String -> BruteInstance
parseFile file = map parseLine (lines file)
    where
        parseLine l = map toInteger (words l)
        toInteger w = do
                        let maybeW = readMaybe w :: Maybe Integer
                        case maybeW of
                            Just w' -> w'
                            Nothing -> -1

-- |'getMaxFeature'
getMaxFeatValue :: BruteInstance -> Integer
getMaxFeatValue = foldl (max) 0 . map (\x -> foldl (max) 0 x )

-- |'vecFeatures'
-- |Está função retorna o zipWithIndex da consolidação das features e a quantidade de features
parVecFeatures :: ChunksOf BruteInstance -> (Integer, [(Integer, Integer)])
parVecFeatures chunks = zipWithIndexBackLen 
    $ mapReduce id (\x y -> sort $ nub (x ++ y)) chunks

-- |'indoFeatPerObj
-- |Esta função retorna informações com relação aos de objetos e as features Exemplo: número de objetos, quantidade máxima e mínima de features por objeto e a soma de features dos objetos
parInfoFeatPerObj :: ChunksOf BruteInstance -> (Integer, Integer, Integer, Integer)
parInfoFeatPerObj chunks = mapReduce specialLen infoFold chunks
    where
        infoFold (x1, x2, x3, x4) (y1, y2, y3, y4) = (x1 + y1, x2+y2, min x3 y3, max x4 y4)
        specialLen x = (1, lenthX, lenthX, lenthX)
            where
                lenthX = len x

-- |'tratarDados'
-- |Esta função troca os labels das features para números no intervado {1, 2.. m} onde m é a quantidade de diferentes features
tratarDados :: Instance -> Dictionary -> Instance
tratarDados dM dict = map (\(i, di) -> (i, sort $ map (\dij -> dict M.! dij) di) ) dM

-- |'parTratarDados'
parTratarDados :: ChunksOf Instance -> Dictionary -> ChunksOf Instance
parTratarDados chunks dict = parmap (\x -> tratarDados x dict ) chunks


-- |##########################################################################################
-- |##########################################################################################




-- |'SIMPLEEVALUATION' 
-- ##################################################################################


-- |'filterxD'
-- |Filtra as linhas da matrix D com relação aos índices i da solução x cujos valores x_i = 1
filterxD :: ObjectLabels -> Instance -> FilteredInstance
filterxD x d = filterxD' x d []
    where
        filterxD' []  _ h   = h
        filterxD'  _ [] h   = h
        filterxD' (x:xs) ((a, b):ds) h
            | x == a        = filterxD'    xs  ds (h ++ [b])
            | otherwise     = filterxD' (x:xs) ds  h

parFilterxD :: ObjectLabels -> ChunksOf Instance -> ChunksOf FilteredInstance
parFilterxD x dCks = parmap (\d -> filterxD x d) dCks 


-- |'filteryd'
filteryd :: FeatureLabels -> FilteredInstanceRow -> Integer
filteryd y d = filteryd' y d 0
    where
        filteryd'     []  _     c = c
        filteryd' (y:ys) []     c = filteryd'    ys    []  (c+1)
        filteryd' (y:ys) (d:ds) c
            | d == y              = filteryd'    ys    ds  (c-1)
            | d > y               = filteryd'    ys (d:ds) (c+1)
            | otherwise           = filteryd' (y:ys)   ds   c

-- |'simpleEvalPar' Avaliação básica distribuída
simpleEvalPar :: ChunksOf Solution -> ChunksOf Instance -> SolutionValue
simpleEvalPar gM dCks = ( mapReduce ( \(gxk, gyk) -> 
      mapReduce ( \di -> filteryd gyk di ) (+) 
      $ filter (/=[]) 
      $ parFilterxD gxk dCks ) (+)
    $ gM )


-- |##################################################################################
-- |##################################################################################




-- | INITIAL SOLUTION
-- |##################################################################################

-- |'initSolution' Initial solution
initSolution :: Int -> Integer -> Integer -> Integer -> Solution
--[([Integer], [Integer])]
initSolution yLen n m k = listSol [] k
    where
        listSol s 1 = [( [ x | x <- [ 1, 2 .. (pObj+rObj) ]], take yLen [y | y <- [ 1,2 .. (pFea+rFea)]] )] ++ s
        listSol s k = listSol ( [( [ x | x <- [ (k-1)*pObj+rObj+1, (k-1)*pObj+rObj+2 .. k*pObj+rObj]], take yLen [y | y <- [(k-1)*pFea+rFea+1,(k-1)*pFea+rFea+2 .. k*pFea+rFea]] )] ++ s) (k-1)
        pObj = n `div` k
        rObj = n `mod` k
        pFea = m `div` k
        rFea = m `mod` k
            
-- |##################################################################################
-- |##################################################################################




-- | Main
-- |##################################################################################
main :: IO()
main = do

    -- |Entrada dos dados
    file <- readFile "teste2.data"
    let 
      dataset = parseFile file
      numCks = 1000 :: Int
      dataChunks  = chunksOf numCks dataset

    -- |Pré-processamento
    let
      (m, featuresDic) = parVecFeatures dataChunks
      dictHash = M.fromList featuresDic
      (n, sumFeat, minFeat, maxFeat) = parInfoFeatPerObj dataChunks
      aveFeat = fromIntegral (sumFeat `div` n)
      k = 10

    print ( (n, m) )
    print ( (aveFeat, minFeat, maxFeat) )

    let 
      dMCks =  parTratarDados (chunksOf numCks $ zipWithIndex dataset) dictHash
      gMatrix = initSolution aveFeat n m k
      gMCks = chunksOf 2 gMatrix

    -- |Cálculo do valor da função objetivo
    print( simpleEvalPar gMCks dMCks  )
    --print( simpleEvalPar gMatrix dMCks  )
