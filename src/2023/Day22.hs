module Day22 where

import Parser
import Utils

input :: IO String
input = readFile "../../inputs/2023/day22/real-input.txt"

exampleInput :: IO String
exampleInput = readFile "../../inputs/2023/day22/example-input-1.txt"

main :: IO ()
main = do
  input <- input
  let answer1 = 0
      answer2 = 0
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
