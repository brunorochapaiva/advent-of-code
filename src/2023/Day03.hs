module Day03 where

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

main :: IO ()
main = do
  input <- readFile "input03.txt"
  let entries = lines input
      answer1 = getSumParts entries
      answer2 = 1
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
