{-# LANGUAGE TupleSections #-}

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
  match' "seeds:" ; parseWhitespace
  many (fmap (,1) parseNatAndSpace)

seedParserPairs :: Parser [(Int, Int)]
seedParserPairs = atomic $ do
  match' "seeds:"
  parseWhitespace
  many $ atomic $ do
    start <- parseNatAndSpace
    range <- parseNatAndSpace
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


intermediateMapParser :: Parser ((Bool, Int, Int) -> [(Bool, Int, Int)])
intermediateMapParser = atomic $ do
  target <- parseNatAndSpace
  source <- parseNatAndSpace
  range  <- parseNatAndSpace
  g      <- intermediateMapParser <|> pure pureMapInterval
  pure (addMapInterval source target range >=> g)

mapParser :: Parser ((Int, Int) -> [(Int, Int)])
mapParser = atomic $ do
  many $ matchIf' (not . isSpace)
  parseWhitespace ; match' "map:" ; parseWhitespace
  f <- intermediateMapParser
  parseWhitespace
  pure (map removeBool . f . addFalse)
  where
    addFalse :: (Int, Int) -> (Bool, Int, Int)
    addFalse (s, r) = (False, s, r)

    removeBool :: (Bool, Int, Int) -> (Int, Int)
    removeBool (_, s, r) = (s, r)

inputParser :: Parser ([ (Int, Int) ], [ (Int, Int) -> [(Int, Int)] ])
inputParser = (,) <$> seedParser <*> many mapParser

inputParserPairs :: Parser ([ (Int, Int) ], [ (Int, Int) -> [(Int, Int)] ])
inputParserPairs = (,) <$> seedParserPairs <*> many mapParser

getMinLocation :: ([ (Int, Int) ], [ (Int, Int) -> [(Int, Int)] ]) -> Int
getMinLocation (seeds, maps) = fst
                             $ minimumBy (compare `on` fst)
                             $ foldl (>>=) seeds maps

input = readFile "input05.txt"

main :: IO ()
main = do
  input <- readFile "input05.txt"
  let entries = input
      answer1 = getMinLocation <$> exec inputParser input
      answer2 = getMinLocation <$> exec inputParserPairs input
  putStrLn $ concat [ "The first answer is " , show answer1 , ".\n"
                    , "The second answer is " , show answer2 , "."
                    ]
