module Main where

import System.Environment
import Data.List.Split
import qualified Data.Map as Map
import Debug.Trace

type Hdl = String
type Text = String
type ListCorpus = [(Hdl, Text)]
type MapCorpus = Map.Map Hdl Text

main :: IO ()
main = do
  (csvFile:lengthCoefficient:_) <- getArgs
  contents <- readFile csvFile
  let myMapCorpus = makeMapCorpus contents
      myCount = length myMapCorpus
      myPairs = makePairsM myMapCorpus ( read lengthCoefficient :: Float )
      myPairCount = length myPairs
  putStrLn $ show myCount ++ " rows"
  putStrLn $ show myPairCount ++ " pairs"

-- http://stackoverflow.com/a/2921380
tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

makeListCorpus :: [Char] -> ListCorpus
makeListCorpus = map (tuplify2 . splitOn "\t") . lines

makeMapCorpus :: [Char] -> MapCorpus
makeMapCorpus = Map.fromList . makeListCorpus
                 
makePairsL :: ListCorpus -> [(Hdl, Hdl)]
makePairsL []  = []
makePairsL (h:hs) = [ (i, j) | i <- [fst h], j <- map fst hs ] ++ makePairsL hs

makePairsM :: MapCorpus -> Float -> [(Hdl, Hdl)]
makePairsM c lc =
    case (Map.keys c) of []     -> []
                         (x:xs) -> [ (i, j) | i <- [x], j <- xs, ( getLengthDiff (Map.lookup i c) (Map.lookup j c) ) < lc ] ++
                                   (makePairsM (Map.delete x c) lc )

getLengthDiff :: Maybe [Char] -> Maybe [Char] -> Float
getLengthDiff (Just a) (Just b) = abs(diff)
    where diff = abs(fromIntegral (length a)) / (fromIntegral (length b)) - 1.0
