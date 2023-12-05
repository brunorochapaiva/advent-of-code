module Utils where

import Data.Maybe (listToMaybe)

headMaybe :: [a] -> Maybe a
headMaybe = listToMaybe

lastMaybe :: [a] -> Maybe a
lastMaybe = listToMaybe . reverse

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = go xs []
  where
    go (x : xs) acc
      | p x       = reverse acc : go xs []
      | otherwise = go xs (x : acc)
    go [] acc = [ reverse acc ]

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
