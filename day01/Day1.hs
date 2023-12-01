module Day1 where

import Data.Char
import Data.List
import Data.Maybe
import Text.Read

extractDigit :: String -> Maybe Char
extractDigit ('o' : 'n' : 'e' : _)             = Just '1'
extractDigit ('t' : 'w' : 'o' : _)             = Just '2'
extractDigit ('t' : 'h' : 'r' : 'e' : 'e' : _) = Just '3'
extractDigit ('f' : 'o' : 'u' : 'r' : _)       = Just '4'
extractDigit ('f' : 'i' : 'v' : 'e' : _)       = Just '5'
extractDigit ('s' : 'i' : 'x' : _)             = Just '6'
extractDigit ('s' : 'e' : 'v' : 'e' : 'n' : _) = Just '7'
extractDigit ('e' : 'i' : 'g' : 'h' : 't' : _) = Just '8'
extractDigit ('n' : 'i' : 'n' : 'e' : _)       = Just '9'
extractDigit (c : _)                           = Just c
extractDigit []                                = Nothing

convertWordsToDigits :: String -> String
convertWordsToDigits = concat . map (maybeToList . extractDigit) . tails

getCalibrationValue :: String -> Maybe Int
getCalibrationValue cs = readMaybe $ map ($ filter isDigit cs) [head, last]

getSumOfCalibrations :: [String] -> Maybe Int
getSumOfCalibrations = fmap sum . sequence . map getCalibrationValue

main :: IO ()
main = do
  input <- readFile "input.txt"
  let entries = lines input
      answer1 = getSumOfCalibrations entries
      answer2 = getSumOfCalibrations $ map convertWordsToDigits entries
  putStrLn $ concat [ "The first calibration value is " , show answer1 , ".\n"
                    , "The second calibration value is " , show answer2 , "."
                    ]
