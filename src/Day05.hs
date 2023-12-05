module Day05 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe

import Parser

seedParser :: Parser [(Int,Int)]
seedParser = atomic $ do
  match' "seeds:"
  parseWhitespace
  many $ do { n <- parseNat ; parseWhitespace ; pure (n,1) }

seedParserPairs :: Parser [(Int, Int)]
seedParserPairs = atomic $ do
  match' "seeds:"
  parseWhitespace
  many $ atomic $ do
    start <- parseNat
    parseWhitespace
    range <- parseNat
    parseWhitespace
    pure (start, range)

pureMapInterval :: (Bool, Int, Int) -> [(Bool, Int, Int)]
pureMapInterval (_, s, r) = pure (True, s, r)

addMapInterval :: Int -> Int -> Int -> (Bool, Int, Int) -> [(Bool, Int, Int)]
addMapInterval source target range (mapped, s, r)
  | mapped            = [ (True, s, r) ]
  | intervalsDisjoint = [ (False, s, r) ]
  | mapCoversInterval = [ (True, target + s - source, r) ]
  | mapOverlapsStart  = [ (True, target + s - source, end - s)
                        , (False, end, e - end) ]
  | mapOverlapsEnd    = [ (False, s, source - s)
                        , (True, target, e - source) ]
  | mapOverlapsMiddle = [ (False, s, source - s)
                        , (True, target, range)
                        , (False, end, e - end) ]
  where
    -- endings are not inclusive
    end = source + range
    e   = s + r

    -- these should be mutually exclusive and exhaustive
    intervalsDisjoint = e <= source || end <= s
    mapCoversInterval = source <= s && e <= end
    mapOverlapsStart  = source <= s && s < end && end < e
    mapOverlapsEnd    = s < source && source < e && e <= end
    mapOverlapsMiddle = s < source && end < e


helper :: Parser ((Bool, Int, Int) -> [(Bool, Int, Int)])
helper = atomic $ do
  target <- parseNat
  parseWhitespace
  source <- parseNat
  parseWhitespace
  range <- parseNat
  parseWhitespace
  let f = addMapInterval source target range
  g <- helper <|> pure pureMapInterval
  pure (f >=> g)

mapParser :: Parser ((Int, Int) -> [(Int, Int)])
mapParser = atomic $ do
  many $ matchIf' (not . isSpace)
  parseWhitespace
  match' "map:"
  parseWhitespace
  f <- helper
  parseWhitespace
  pure (map removeBool . f . addFalse)
  where
    addFalse :: (Int, Int) -> (Bool, Int, Int)
    addFalse (s, r) = (False, s, r)

    removeBool :: (Bool, Int, Int) -> (Int, Int)
    removeBool (_, s, r) = (s, r)

inputParser :: Parser ([ (Int, Int) ], [ (Int, Int) -> [(Int, Int)] ])
inputParser = do
  seeds <- seedParser
  maps  <- many mapParser
  pure (seeds, maps)

inputParserPairs :: Parser ([ (Int, Int) ], [ (Int, Int) -> [(Int, Int)] ])
inputParserPairs = do
  seeds <- seedParserPairs
  maps  <- many mapParser
  pure (seeds, maps)

getMinLocation :: ([ (Int, Int) ], [ (Int, Int) -> [(Int, Int)] ]) -> Int
getMinLocation (seeds, maps) = fst
                             $ minimumBy (compare `on` fst)
                             $ foldl (>>=) seeds maps

input = readFile "input05.txt"

f :: String -> Int
f s = length $ snd $ fromJust $ fst $ parse inputParser s

f1 :: String -> (Int, Int) -> [(Int, Int)]
f1 s = head $ snd $ fromJust $ fst $ parse inputParser s

f2 :: String -> (Int, Int) -> [(Int, Int)]
f2 s = head $ tail $ snd $ fromJust $ fst $ parse inputParser s

main :: IO ()
main = do
  input <- readFile "input05.txt"
  let entries = input
      answer1 = fmap getMinLocation $ fst $ parse inputParser input
      answer2 = fmap getMinLocation $ fst $ parse inputParserPairs input
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
