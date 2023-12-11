module Day08 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

import Parser
import Utils

data Direction = L | R
  deriving Show

nameParser :: Parser String
nameParser = atomic $ replicateM 3 (matchIf' isAlphaNum)

nodeParser :: Parser (String, String, String)
nodeParser = atomic $ do
  node <- nameParser
  parseWhitespace
  matchChar' '='
  parseWhitespace
  matchChar' '('
  left <- nameParser
  matchChar' ','
  parseWhitespace
  right <- nameParser
  matchChar' ')'
  parseWhitespace
  pure (node, left, right)

directionParser :: Parser Direction
directionParser = (matchChar' 'L' >> pure L) <|> (matchChar' 'R' >> pure R)

inputParser :: Parser ([Direction], [(String, String, String)])
inputParser = do
  dirs  <- many directionParser
  parseWhitespace
  nodes <- many nodeParser
  pure (dirs, nodes)

countSteps :: ([Direction], [(String, String, String)]) -> Maybe Int
countSteps (dirs, nodes) = go (cycle dirs) "AAA" (Just 0)
  where
    go :: [Direction] -> String -> Maybe Int -> Maybe Int
    go (d : dirs) node n
      | node == "ZZZ" = n
      | otherwise     = do
          (_, left, right) <- find ((==node) . fst3) nodes
          case d of
            L -> go dirs left  ((1+) <$> n)
            R -> go dirs right ((1+) <$> n)

countGhostlySteps :: ([Direction], [(String, String, String)]) -> Maybe Int
countGhostlySteps (dirs, nodes) = fmap (foldr lcm 1)
                                $ mapM (go (cycle dirs) (Just 0))
                                $ filter ((=='A') . last)
                                $ map fst3 nodes
  where
    go :: [Direction] -> Maybe Int -> String -> Maybe Int
    go (d : dirs) n node
      | last node == 'Z' = n
      | otherwise        = do
          (_, left, right) <- find ((==node) . fst3) nodes
          case d of
            L -> go dirs ((1+) <$> n) left
            R -> go dirs ((1+) <$> n) right

input :: IO String
input = readFile "../../inputs/2023/day08/real-input.txt"

exampleInput1 :: IO String
exampleInput1 = readFile "../../inputs/2023/day08/example-input-1.txt"

exampleInput2 :: IO String
exampleInput2 = readFile "../../inputs/2023/day08/example-input-2.txt"

main :: IO ()
main = do
  input <- input
  let answer1 = countSteps =<< exec inputParser input
      answer2 = countGhostlySteps =<< exec inputParser input
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
