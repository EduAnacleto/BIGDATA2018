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



type ChunksOf a = [a]


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
flatMap :: [[Integer]] -> [Integer]
flatMap matrix = foldl (++) [] matrix

-- |'distinctFlatMap'
distinctFlatMap :: [[Integer]] -> [Integer]
distinctFlatMap  = sort . nub . flatMap


zipWithIndexBackLen :: [b] -> (Integer, [(b, Integer)])
zipWithIndexBackLen x = zipWIBL x [1,2..] []
    where
        zipWIBL (x:[]) (y:_) h = (y, h++[(x, y)])
        zipWIBL (x:xs) (y:ys) h = zipWIBL xs ys (h++[(x, y)])

-- |'zipWithIndex'
zipWithIndex :: [b] -> [(Integer, b)]
zipWithIndex = zip [1,2..]


-- | ##########################################################################################
-- | ##########################################################################################




-- |'PREPROCESSING FUNCTIONS
-- |##########################################################################################

-- | Leitura de arquivo
-- |'parseFile' parses a space separated file to a list of lists of Integer
parseFile :: String -> [[Integer]]
parseFile file = map parseLine (lines file)
    where
        parseLine l = map toInteger (words l)
        toInteger w = do
                        let maybeW = readMaybe w :: Maybe Integer
                        case maybeW of
                            Just w' -> w'
                            Nothing -> -1

-- |'getMaxFeature'
getMaxFeatValue :: [[Integer]] -> Integer
getMaxFeatValue = foldl (max) 0 . map (\x -> foldl (max) 0 x )

-- 
vecFeatures :: ChunksOf [[Integer]] -> (Integer, [(Integer,Integer)])
vecFeatures chunks = zipWithIndexBackLen $ mapReduce (\x -> x) (\x y -> sort $ nub (x ++ y)) chunks


infoFeatPerObj :: ChunksOf [[Integer]] -> (Integer, Integer, Integer, Integer)
infoFeatPerObj chunks = mapReduce specialLen infoFold chunks
    where
        infoFold (x1, x2, x3, x4) (y1, y2, y3, y4) = (x1 + y1, x2+y2, min x3 y3, max x4 y4)
        specialLen x = (1, lenthX, lenthX, lenthX)
            where
                lenthX = len x

tratarDados :: [(Integer, [Integer])] -> M.HashMap Integer Integer -> [(Integer, [Integer])]
tratarDados dM mapaHash = map (\(i, di) -> (i, sort $ map (\dij -> mapaHash M.! dij) di) ) dM

parTratarDados :: ChunksOf [(Integer, [Integer])] -> M.HashMap Integer Integer -> ChunksOf [(Integer, [Integer])]
parTratarDados chunks mapaHash = parmap (\x -> tratarDados x mapaHash ) chunks


-- |##########################################################################################
-- |##########################################################################################




-- |'SIMPLEEVALUATION' 
-- ##################################################################################

parIntersect :: [Integer] -> ChunksOf [(Integer, [Integer])] -> ChunksOf [[Integer]]
parIntersect x dCks = parmap (\d -> filterxD x d) dCks 

-- |'filterxD'
filterxD :: [Integer] -> [(Integer, [Integer])] -> [[Integer]]
filterxD x d = filterxD' x d []
    where
        filterxD' []  _ h   = h
        filterxD'  _ [] h   = h
        filterxD' (x:xs) ((a, b):ds) h
            | x == a        = filterxD'    xs  ds (h ++ [b])
            | otherwise     = filterxD' (x:xs) ds  h

-- |'filteryd'
filteryd :: [Integer] -> [Integer] -> Integer
filteryd y d = filteryd' y d 0
    where
        filteryd'     []  _     c = c
        filteryd' (y:ys) []     c = filteryd'    ys    []  (c+1)
        filteryd' (y:ys) (d:ds) c
            | d == y              = filteryd'    ys    ds  (c-1)
            | d > y               = filteryd'    ys (d:ds) (c+1)
            | otherwise           = filteryd' (y:ys)   ds   c

-- |'simpleEvalPar' Avaliação básica distribuída
simpleEvalPar :: [([Integer], [Integer])] -> ChunksOf [(Integer, [Integer])] -> Integer
simpleEvalPar gM dCks = foldl1' (+) $ map ( \(gxk, gyk) -> mapReduce (\di -> filteryd gyk di) (+) (parIntersect gxk dCks)) gM

-- |##################################################################################
-- |##################################################################################




-- | INITIAL SOLUTION
-- |##################################################################################

-- |'initSolution' Initial solution
initSolution :: Int -> Integer -> Integer -> Integer -> [([Integer], [Integer])]
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

    file <- readFile "teste2.data"
    let 
      dataset = parseFile file
      numCks = 1000 :: Int
      dataChunks  = chunksOf numCks dataset

    let
      (m, featuresDic) = vecFeatures dataChunks
      dictHash = M.fromList featuresDic
      (n, sumFeat, minFeat, maxFeat) = infoFeatPerObj dataChunks
      k = 10

    let
      aveFeat = fromIntegral (sumFeat `div` n)

    print ( (n, m) )
    print ( (aveFeat, minFeat, maxFeat) )
    print ( featuresDic )

    -- | Parallel File
    let 
      dMCks =  parTratarDados (chunksOf numCks $ zipWithIndex dataset) dictHash
      gMatrix = initSolution aveFeat n m k

    print( simpleEvalPar gMatrix dMCks  )
