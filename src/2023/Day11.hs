{-# LANGUAGE TupleSections #-}

module Day11 where

import Data.List

import Parser
import Utils

doubleIfEmpty :: [Char] -> [[Char]] -> [[Char]]
doubleIfEmpty l ls
  | all (== '.') l = l : l : ls
  | otherwise      = l : ls

expandSingleDirection :: [[Char]] -> [[Char]]
expandSingleDirection = foldr doubleIfEmpty []

expandBothDirections :: [[Char]] -> [[Char]]
expandBothDirections = transpose . expandSingleDirection
                     . transpose . expandSingleDirection

taxicabDistance :: (Int, Int) -> (Int, Int) -> Int
taxicabDistance (x,y) (u,v) = abs (x - u) + abs (y - v)

getGalaxies :: [[Char]] -> [(Int, Int)]
getGalaxies = concat . zipWith f [0..]
  where
    f :: Int -> [Char] -> [(Int, Int)]
    f n l = map ((n,) . fst) $ filter ((== '#') . snd) $ zip [0..] l

sumDistances :: [[Char]] -> Int
sumDistances ls = let cs = getGalaxies $ expandBothDirections ls
                   in sum [ taxicabDistance c c' | c <- cs, c' <- cs, c /= c'] `div` 2

getExpandedRows :: [[Char]] -> [Int]
getExpandedRows = map fst . filter (all (== '.') . snd) . zip [0..]

getExpandedCols :: [[Char]] -> [Int]
getExpandedCols = getExpandedRows . transpose

expandedTaxicabDistance :: [Int] -> [Int] -> (Int, Int) -> (Int, Int) -> Int
expandedTaxicabDistance rs cs (x,y) (u,v) =
  abs (x - u) + extraRows + abs (y - v) + extraCols
  where
   isBetween l u x = (l < x && x < u) || (u < x && x < l)

   rsBetween = filter (isBetween x u) rs
   csBetween = filter (isBetween y v) cs

   extraRows = 999999 * length rsBetween
   extraCols = 999999 * length csBetween

sumOldDistances :: [[Char]] -> Int
sumOldDistances ls =
  sum [ expandedTaxicabDistance rs cs c c' | c <- coords, c' <- coords, c /= c' ] `div` 2
  where
   coords = getGalaxies ls
   rs     = getExpandedRows ls
   cs     = getExpandedCols ls

input :: IO String
input = readFile "../../inputs/2023/day11/real-input.txt"

exampleInput :: IO String
exampleInput = readFile "../../inputs/2023/day11/example-input-1.txt"

main :: IO ()
main = do
  input <- lines <$> input
  let answer1 = sumDistances input
      answer2 = sumOldDistances input
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
