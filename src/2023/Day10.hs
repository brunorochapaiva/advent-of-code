module Day10 where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.ST.Strict
import Data.Bifoldable
import Data.List
import Data.Maybe
import Data.STRef.Strict

import Parser
import Utils

getStart :: [[Char]] -> Maybe (Int, Int)
getStart pipes = do
  rowContainingStart <- find (elem 'S' . snd) $ zip [0..] pipes
  col <- elemIndex 'S' $ snd rowContainingStart
  pure (fst rowContainingStart, col)

getTile :: [[Char]] -> (Int, Int) -> Char
getTile tiles (row, col) = fromMaybe '.' ((tiles !? row) >>= (!? col))

getPossibleNextCoords :: (Int, Int) -> Char -> [(Int, Int)]
getPossibleNextCoords (y,x) '|' = [(y-1,x), (y+1,x)]
getPossibleNextCoords (y,x) '-' = [(y,x-1), (y,x+1)]
getPossibleNextCoords (y,x) 'L' = [(y-1,x), (y,x+1)]
getPossibleNextCoords (y,x) 'J' = [(y-1,x), (y,x-1)]
getPossibleNextCoords (y,x) '7' = [(y+1,x), (y,x-1)]
getPossibleNextCoords (y,x) 'F' = [(y+1,x), (y,x+1)]
getPossibleNextCoords (y,x) 'S' = [(y-1,x),(y,x+1),(y+1,x),(y,x-1)]
getPossibleNextCoords (y,x) _   = []

getNextCoord :: (Int, Int) -> (Int, Int) -> Char -> (Int, Int)
getNextCoord prev curr tile = head $ delete prev (getPossibleNextCoords curr tile)

tileMatchesMovement :: (Int, Int) -> (Int, Int) -> Char -> Bool
tileMatchesMovement prev curr tile = prev `elem` getPossibleNextCoords curr tile

followPipe :: [[Char]] -> [(Int, Int)] -> (Int, Int) -> Maybe [(Int, Int)]
followPipe tiles path curr = let tile = getTile tiles curr in case tile of
  'S' -> pure path
  _   -> do guard $ tileMatchesMovement (head path) curr tile
            followPipe tiles (curr : path) (getNextCoord (head path) curr tile)

findPipePath :: [[Char]] -> (Int, Int) -> Maybe [(Int, Int)]
findPipePath tiles start =
  asum (followPipe tiles [start] <$> getPossibleNextCoords start 'S')

getStepsToHalf :: [[Char]] -> Maybe Int
getStepsToHalf tiles = do
  start <- getStart tiles
  path  <- findPipePath tiles start
  pure (length path `div` 2)

countTilesInside :: [[Char]] -> Maybe Int
countTilesInside tiles = Nothing

input :: IO String
input = readFile "../../inputs/2023/day10/real-input.txt"

exampleInput :: IO String
exampleInput = readFile "../../inputs/2023/day10/example-input-1.txt"

main :: IO ()
main = do
  input <- lines <$> input
  let answer1 = getStepsToHalf input
      answer2 = countTilesInside input
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
