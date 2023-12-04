{-# LANGUAGE LambdaCase #-}

module Day02 where

import Control.Applicative
import Data.Foldable

import Parser

data Set  = Set { red :: Int, green :: Int, blue :: Int } deriving Show

parseColour :: String -> Parser Int
parseColour col = atomic $ do
  n <- parseNat
  parseWhitespace
  match' col
  pure n

parseRed :: Parser (Set -> Set)
parseRed = (\n s -> s{ red = n}) <$> parseColour "red"

parseGreen :: Parser (Set -> Set)
parseGreen = (\n s -> s{ green = n}) <$> parseColour "green"

parseBlue :: Parser (Set -> Set)
parseBlue = (\n s -> s{ blue = n}) <$> parseColour "blue"

-- assumes no whitespace at the start, consumes trailing whitespace
parseSet :: Parser Set
parseSet = fmap ($ Set 0 0 0) go
  where
    go :: Parser (Set -> Set)
    go = do
      f <- parseRed <|> parseGreen <|> parseBlue
      g <-     (matchChar' ',' >> parseWhitespace >> go)
           <|> (parseWhitespace >> pure id)
      pure $ g . f

data Game = Game { gameID :: Int, sets :: [Set] } deriving Show

-- assumes no whitespace at the start, consumes trailing whitespace
parseGame :: Parser Game
parseGame = do
  match' "Game"
  parseWhitespace
  gameID <- parseNat
  matchChar' ':'
  parseWhitespace
  sets <- go
  pure $ Game gameID sets
  where
    go :: Parser [Set]
    go = do
      s  <- parseSet
      ss <-     (matchChar' ';' >> parseWhitespace >> go)
            <|> (parseWhitespace >> pure [])
      pure $ s : ss

validityPredicate :: Game -> Bool
validityPredicate (Game id sets) = all helper sets
  where
    helper (Set r g b) = r <= 12 && g <= 13 && b <= 14

getMinSet :: Game -> Set
getMinSet = foldr (\s1 s2 -> Set (max (red s1) (red s2))
                                 (max (green s1) (green s2))
                                 (max (blue s1) (blue s2)))
                  (Set 0 0 0)
          . sets

sumValidIDS :: [String] -> Maybe Int
sumValidIDS = fmap (sum . map gameID . filter validityPredicate)
            . mapM (fst . parse parseGame)

sumPowers :: [String] -> Maybe Int
sumPowers = fmap (sum . map ((\case (Set r g b) -> r * g * b) . getMinSet))
          . mapM (fst . parse parseGame)

main :: IO ()
main = do
  input <- readFile "input02.txt"
  let entries = lines input
      answer1 = sumValidIDS entries
      answer2 = sumPowers entries
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
