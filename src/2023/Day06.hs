module Day06 where

import Control.Applicative


import Parser
import Utils

timeParser :: Parser [Int]
timeParser = atomic $ do
  match' "Time:"
  parseWhitespace
  many $ do { n <- parseNat ; parseWhitespace ; pure n }

distanceParser :: Parser [Int]
distanceParser = atomic $ do
  match' "Distance:"
  parseWhitespace
  many $ do { n <- parseNat ; parseWhitespace ; pure n }

racesParser :: Parser [(Int, Int)]
racesParser = zip <$> timeParser <*> distanceParser

raceParser :: Parser (Int, Int)
raceParser = atomic $ do
  match' "Time:"
  parseWhitespace
  ns <- many $ do {n <- parseNat ; parseWhitespace ; pure n}
  let time = read $ concatMap show ns
  match' "Distance:"
  parseWhitespace
  ns <- many $ do {n <- parseNat ; parseWhitespace ; pure n}
  let distance = read $ concatMap show ns
  pure (time, distance)

countWins :: (Int, Int) -> Int
countWins (time, distance) = length
                           $ filter (> distance)
                           $ map (\s -> (time - s) * s) [0 .. time]

multWins :: [(Int, Int)] -> Int
multWins = product . map countWins

input :: IO String
input = readFile "../../inputs/2023/day06/real-input.txt"

exampleInput :: IO String
exampleInput = readFile "../../inputs/2023/day06/example-input-1.txt"

main :: IO ()
main = do
  input <- input
  let answer1 = fmap multWins $ fst $ parse racesParser input
      answer2 = fmap countWins $ fst $ parse raceParser input
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
