{-# LANGUAGE TupleSections #-}

module Day03 where

import Control.Arrow
import Data.Char (isDigit)
import Data.List
import Data.Maybe

import Utils

getNumberAndCols :: String -> [(Int, (Int, Int))]
getNumberAndCols = go 0
  where
    go :: Int -> String -> [(Int, (Int, Int))]
    go _ []     = []
    go n (c:cs)
      | isDigit c = let (ds, cs') = span isDigit cs
                        m         = length ds
                        num       = read (c : ds)
                     in (num, (n, n + m)) : go (n + m + 1) cs'
      | otherwise = go (n + 1) cs


getNumberAndCoords :: [String] -> [(Int, (Int, Int, Int))]
getNumberAndCoords = concat
                   . zipWith (\n cs -> map (\(num, (c, c')) -> (num, (n, c, c'))) (getNumberAndCols cs)) [0 ..]

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c || c == '.')

isAdjacentToSymbol :: [String] -> (Int, Int, Int) -> Bool
isAdjacentToSymbol xs (row, col1, col2) = before || after || above || below
  where
    before = any isSymbol ((xs !? row) >>= (!? (col1 - 1)))
    after  = any isSymbol ((xs !? row) >>= (!? (col2 + 1)))
    above  = any (any isSymbol . take (col2 - col1 + 3) . drop (col1 - 1)) (xs !? (row - 1))
    below  = any (any isSymbol . take (col2 - col1 + 3) . drop (col1 - 1)) (xs !? (row + 1))

getSumParts :: [String] -> Int
getSumParts cs = sum
               $ map fst
               $ filter (isAdjacentToSymbol cs . snd)
               $ getNumberAndCoords cs

isAdjacentToCoord :: Int -> Int -> (Int, Int, Int) -> Bool
isAdjacentToCoord r c (row, col1, col2) =
  before || after || above || below
  where
    before = col1 == c + 1 && row == r
    after  = col2 == c - 1 && row == r
    above  = col1 - 1 <= c && c <= col2 + 1 && row - 1 == r
    below  = col1 - 1 <= c && c <= col2 + 1 && row + 1 == r

getGearRatio :: [(Int, (Int, Int, Int))] -> (Int, Int) -> Int
getGearRatio nums (row, col)
  | length adjs == 2 = product $ map fst adjs
  | otherwise        = 0
  where
    adjs = filter (isAdjacentToCoord row col . snd) nums

getGearCoords :: [String] -> [(Int,Int)]
getGearCoords []     = []
getGearCoords (l:ls) =
     map ((0,) . fst) (filter ((=='*') . snd) (zip [0..] l))
  ++ map (first (1+)) (getGearCoords ls)

sumGearRatios :: [String] -> Int
sumGearRatios cs = sum
                 $ map (getGearRatio (getNumberAndCoords cs))
                 $ getGearCoords cs


input :: IO String
input = readFile "../../inputs/2023/day03/real-input.txt"

exampleInput :: IO String
exampleInput = readFile "../../inputs/2023/day03/example-input-1.txt"

main :: IO ()
main = do
  input <- input
  let entries = lines input
      answer1 = getSumParts entries
      answer2 = sumGearRatios entries
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
