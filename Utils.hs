module Utils where

import Data.Maybe (listToMaybe)

headMaybe :: [a] -> Maybe a
headMaybe = listToMaybe

lastMaybe :: [a] -> Maybe a
lastMaybe = listToMaybe . reverse
