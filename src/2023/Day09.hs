module Day09 where

import Control.Applicative

import Parser
import Utils

inputParser :: Parser [[Int]]
inputParser = many $ do
  ns <- many parseIntAndSpace'
  parseNewline
  pure ns

getDifferences :: [Int] -> [Int]
getDifferences (x : y : zs) = (y - x) : getDifferences (y : zs)
getDifferences [x]          = []
getDifferences []           = []

predictNext :: [Int] -> Int
predictNext = sum
            . map last
            . takeWhile (any (/= 0))
            . iterate getDifferences

predictBefore :: [Int] -> Int
predictBefore = foldr (flip subtract . head) 0
              . takeWhile (any (/= 0))
              . iterate getDifferences

sumPredictions :: [[Int]] -> Int
sumPredictions = sum . map predictNext

sumBeforePredictions :: [[Int]] -> Int
sumBeforePredictions = sum . map predictBefore

input :: IO String
input = readFile "../../inputs/2023/day09/real-input.txt"

exampleInput :: IO String
exampleInput = readFile "../../inputs/2023/day09/example-input-1.txt"

main :: IO ()
main = do
  input <- input
  let answer1 = sumPredictions <$> exec inputParser input
      answer2 = sumBeforePredictions <$> exec inputParser input
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
