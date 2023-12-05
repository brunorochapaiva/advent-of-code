module Day04 where

import Control.Applicative
import Data.Function
import Data.List

import Parser
import Utils

data Card = Card {cardID :: Int, pickedNums :: [Int], winningNums :: [Int]}
  deriving Show

cardParser :: Parser Card
cardParser = do
  match' "Card"
  parseWhitespace
  cardID <- parseNat
  parseWhitespace
  matchChar' ':'
  parseWhitespace
  winning <- parseNums
  matchChar' '|'
  parseWhitespace
  picked <- parseNums
  pure $ Card cardID picked winning
  where
    parseNums :: Parser [Int]
    parseNums = many $ do { n <- parseNat; parseWhitespace; pure n }

readCards :: [String] -> Maybe [Card]
readCards = mapM (fst . parse cardParser)

getMatches :: Card -> Int
getMatches (Card _ picked winning) = length (filter (`elem` winning) picked)

getPoints :: Card -> Int
getPoints c = 2 ^ getMatches c `div` 2

countPoints :: [Card] -> Int
countPoints = sum . map getPoints

countCards :: [Card] -> Int
countCards cs = sum $ go (reverse cs) []
  where
    go :: [Card] -> [Int] -> [Int]
    go []     acc = acc
    go (c:cs) acc = go cs $ (1 + sum (take (getMatches c) acc)) : acc

main :: IO ()
main = do
  input <- readFile "input04.txt"
  let entries = lines input
      answer1 = countPoints <$> readCards entries
      answer2 = countCards <$> readCards entries
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
