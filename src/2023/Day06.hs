module Day06 where

import Control.Applicative

import Parser
import Utils

timeParser :: Parser [Int]
timeParser = atomic $ do
  match' "Time:"
  parseWhitespace
  many parseNatAndSpace

distanceParser :: Parser [Int]
distanceParser = atomic $ do
  match' "Distance:"
  parseWhitespace
  many parseNatAndSpace

racesParser :: Parser [(Int, Int)]
racesParser = zip <$> timeParser <*> distanceParser

raceParser :: Parser (Int, Int)
raceParser = atomic $ do
  match' "Time:"
  parseWhitespace
  ns <- many parseNatAndSpace
  match' "Distance:"
  parseWhitespace
  ns <- many parseNatAndSpace
  pure (read $ concatMap show ns, read $ concatMap show ns)

countWins :: (Int, Int) -> Int
countWins (time, distance) =
  sum [ 1 | s <- [0 .. time], (time - s) * s > distance ]

multWins :: [(Int, Int)] -> Int
multWins = product . map countWins

input :: IO String
input = readFile "../../inputs/2023/day06/real-input.txt"

exampleInput :: IO String
exampleInput = readFile "../../inputs/2023/day06/example-input-1.txt"

main :: IO ()
main = do
  input <- input
  let answer1 = multWins  <$> exec racesParser input
      answer2 = countWins <$> exec raceParser input
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
