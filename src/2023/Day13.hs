module Day13 where

import Control.Applicative
import Control.Arrow
import Data.List

import Parser
import Utils

findReflectionPoints :: [Char] -> [Int]
findReflectionPoints ts = map (length . fst)
                        $ filter (and . uncurry (zipWith (==)))
                        $ map (first reverse . (`splitAt` ts)) [1 .. length ts - 1]

findSmudgedReflectionPoints :: [Char] -> [Int]
findSmudgedReflectionPoints ts = map (length . fst)
                               $ filter ((== 1) . length . filter not . uncurry (zipWith (==)))
                               $ map (first reverse . (`splitAt` ts)) [1 .. length ts - 1]

findReflectionLine :: [[Char]] -> Maybe Int
findReflectionLine = headMaybe
                   . foldr (intersect . findReflectionPoints) [1..]

findSmudgedReflectionLine :: [[Char]] -> Maybe Int
findSmudgedReflectionLine ts = asum $ map tryArrangement [0 .. length ts - 1]
  where
    tryArrangement :: Int -> Maybe Int
    tryArrangement i = headMaybe
                     $ foldr intersect [1..]
                     $ zipWith ($) (getArrangement i) ts

    getArrangement :: Int -> [ [Char] -> [Int] ]
    getArrangement i = replicate i findReflectionPoints
                       ++ [ findSmudgedReflectionPoints ]
                       ++ replicate (length ts - i - 1) findReflectionPoints

summariseIndividual :: [[Char]] -> Maybe Int
summariseIndividual ts =  findReflectionLine ts
                      <|> ((100 *) <$> findReflectionLine (transpose ts))

summariseSmudgedIndividual :: [[Char]] -> Maybe Int
summariseSmudgedIndividual ts =  findSmudgedReflectionLine ts
                             <|> ((100 *) <$> findSmudgedReflectionLine (transpose ts))

summariseAll :: [[[Char]]] -> Maybe Int
summariseAll = fmap sum . mapM summariseIndividual

summariseSmudgedAll :: [[[Char]]] -> Maybe Int
summariseSmudgedAll = fmap sum . mapM summariseSmudgedIndividual

input :: IO String
input = readFile "../../inputs/2023/day13/real-input.txt"

exampleInput :: IO String
exampleInput = readFile "../../inputs/2023/day13/example-input-1.txt"

main :: IO ()
main = do
  input <- splitWhen null . lines <$> input
  let answer1 = summariseAll input
      answer2 = summariseSmudgedAll input
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
