module Main where

import Data.Maybe         (fromMaybe)
import System.Environment (getArgs)
import System.Exit        (exitFailure, exitSuccess)
import Text.Read          (readMaybe)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

main :: IO ()
main = getArgs >>= picker

picker :: [String] -> IO ()
picker []     = usage
picker ["-h"] = usage
picker ["-a"] = mapM_ runDay [1..25]
picker days   = mapM_ (runDay . fromMaybe (-1) . readMaybe) days

runDay :: Int -> IO ()
runDay  1 = putStrLn "Day 01" >> Day01.main >> putStrLn "----"
runDay  2 = putStrLn "Day 02" >> Day02.main >> putStrLn "----"
runDay  3 = putStrLn "Day 03" >> Day03.main >> putStrLn "----"
runDay  4 = putStrLn "Day 04" >> Day04.main >> putStrLn "----"
runDay  5 = putStrLn "Day 05" >> Day05.main >> putStrLn "----"
runDay  6 = putStrLn "Day 06" >> Day06.main >> putStrLn "----"
runDay  7 = putStrLn "Day 07" >> Day07.main >> putStrLn "----"
runDay  8 = putStrLn "Day 08" >> Day08.main >> putStrLn "----"
runDay  9 = putStrLn "Day 09" >> Day09.main >> putStrLn "----"
runDay 10 = putStrLn "Day 10" >> Day10.main >> putStrLn "----"
runDay 11 = putStrLn "Day 11" >> Day11.main >> putStrLn "----"
runDay 12 = putStrLn "Day 12" >> Day12.main >> putStrLn "----"
runDay 13 = putStrLn "Day 13" >> Day13.main >> putStrLn "----"
runDay 14 = putStrLn "Day 14" >> Day14.main >> putStrLn "----"
runDay 15 = putStrLn "Day 15" >> Day15.main >> putStrLn "----"
runDay 16 = putStrLn "Day 16" >> Day16.main >> putStrLn "----"
runDay 17 = putStrLn "Day 17" >> Day17.main >> putStrLn "----"
runDay 18 = putStrLn "Day 18" >> Day18.main >> putStrLn "----"
runDay 19 = putStrLn "Day 19" >> Day19.main >> putStrLn "----"
runDay 20 = putStrLn "Day 20" >> Day20.main >> putStrLn "----"
runDay 21 = putStrLn "Day 21" >> Day21.main >> putStrLn "----"
runDay 22 = putStrLn "Day 22" >> Day22.main >> putStrLn "----"
runDay 23 = putStrLn "Day 23" >> Day23.main >> putStrLn "----"
runDay 24 = putStrLn "Day 24" >> Day24.main >> putStrLn "----"
runDay 25 = putStrLn "Day 25" >> Day25.main >> putStrLn "----"
runDay _  = exitFailure

usage :: IO ()
usage = putStrLn "Usage: aoc [-v] [-a] [day ..]" >> exitSuccess
