module Day07 where

import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe

import Parser
import Utils

cardParser :: Parser Int
cardParser =   (matchChar' 'A' >> pure 14)
           <|> (matchChar' 'K' >> pure 13)
           <|> (matchChar' 'Q' >> pure 12)
           <|> (matchChar' 'J' >> pure 11)
           <|> (matchChar' 'T' >> pure 10)
           <|> (matchChar' '9' >> pure  9)
           <|> (matchChar' '8' >> pure  8)
           <|> (matchChar' '7' >> pure  7)
           <|> (matchChar' '6' >> pure  6)
           <|> (matchChar' '5' >> pure  5)
           <|> (matchChar' '4' >> pure  4)
           <|> (matchChar' '3' >> pure  3)
           <|> (matchChar' '2' >> pure  2)

wildcardParser :: Parser Int
wildcardParser =   (matchChar' 'A' >> pure 13)
               <|> (matchChar' 'K' >> pure 12)
               <|> (matchChar' 'Q' >> pure 11)
               <|> (matchChar' 'T' >> pure 10)
               <|> (matchChar' '9' >> pure  9)
               <|> (matchChar' '8' >> pure  8)
               <|> (matchChar' '7' >> pure  7)
               <|> (matchChar' '6' >> pure  6)
               <|> (matchChar' '5' >> pure  5)
               <|> (matchChar' '4' >> pure  4)
               <|> (matchChar' '3' >> pure  3)
               <|> (matchChar' '2' >> pure  2)
               <|> (matchChar' 'J' >> pure  1)

handsParser :: Parser [([Int], Int)]
handsParser = many $ atomic $ do
  cards <- many cardParser
  parseWhitespace
  bid <- parseNatAndSpace
  pure (cards, bid)

wildhandsParser :: Parser[([Int], Int)]
wildhandsParser = many $ atomic $ do
  cards <- many wildcardParser
  parseWhitespace
  bid <- parseNatAndSpace
  pure (cards, bid)

handScore :: [Int] -> (Int, [Int])
handScore hand = (fromJust $ elemIndex True handTypes, hand)
  where
    groups     = group $ sort hand :: [[Int]]

    handTypes =
      [ length groups == 5                                                  -- isHighCard
      , length groups == 4                                                  -- isOnePair
      , length (filter ((== 2) . length) groups) == 2                       -- isTwoPair
      , length (filter ((== 3) . length) groups) == 1 && length groups == 3 -- isThreeOfAKind
      , length (filter ((== 3) . length) groups) == 1 && length groups == 2 -- isFullHouse
      , length (filter ((== 4) . length) groups) == 1 && length groups == 2 -- isFourOfAKind
      , length groups == 1                                                  -- isFiveOfAKind
      ]

fillJoker :: Int -> [Int]
fillJoker 1 = [2..13]
fillJoker n = [n]

wildhandScore :: [Int] -> (Int, [Int])
wildhandScore hand = (maximum $ map (fst . handScore) $ mapM fillJoker hand, hand)

countWinnings :: [([Int], Int)] -> Int
countWinnings = sum . zipWith (*) [1..] . map snd . sortOn (handScore . fst)

countWildWinnings :: [([Int], Int)] -> Int
countWildWinnings = sum . zipWith (*) [1..] . map snd . sortOn (wildhandScore . fst)

input :: IO String
input = readFile "../../inputs/2023/day07/real-input.txt"

exampleInput :: IO String
exampleInput = readFile "../../inputs/2023/day07/example-input-1.txt"

main :: IO ()
main = do
  input <- input
  let answer1 = countWinnings     <$> exec handsParser input
      answer2 = countWildWinnings <$> exec wildhandsParser input
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
