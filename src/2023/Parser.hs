{-# LANGUAGE LambdaCase , TupleSections #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe

newtype Parser a = Parser { parse :: String -> (Maybe a, String) }

exec :: Parser a -> String -> Maybe a
exec p s = fst (parse p s)

instance Functor Parser where
  fmap f p = Parser $ \inp -> let (mx, out) = parse p inp
                               in (fmap f mx, out)

instance Applicative Parser where
  pure x = Parser (Just x,)

  -- If the first parser fails, don't execute the second parser
  pf <*> px = Parser $ \inp -> let (mf, mid) = parse pf inp
                                   (mx, out) = parse px mid
                                in maybe (Nothing, mid)
                                         (\f -> (fmap f mx, out))
                                         mf

instance Monad Parser where
  px >>= f = Parser $ \inp -> let (mx, mid) = parse px inp
                               in maybe (Nothing, mid) (\x -> parse (f x) mid) mx

instance Alternative Parser where
  empty = Parser (Nothing,)

  (<|>) p1 p2 = Parser $ \inp -> let (mx, mid) = parse p1 inp
                                  in if isJust mx then (mx, mid)
                                                  else parse p2 mid

instance MonadPlus Parser where

atomic :: Parser a -> Parser a
atomic p = Parser $ \inp -> let (mx, out) = parse p inp
                             in (mx, maybe inp (const out) mx)

matchIf :: (Char -> Bool) -> Parser Char
matchIf p = Parser $ \case (c : cs) -> (guard (p c) >> Just c, cs)
                           []       -> (Nothing, [])

matchIf' :: (Char -> Bool) -> Parser Char
matchIf' = atomic . matchIf

matchChar :: Char -> Parser Char
matchChar c = matchIf (== c)

matchChar' :: Char -> Parser Char
matchChar' c = matchIf' (== c)

match :: String -> Parser String
match = mapM matchChar

match' :: String -> Parser String
match' = mapM matchChar'

parseNat :: Parser Int
parseNat = do
  ns <- many $ matchIf' isDigit
  guard (ns /= [])
  pure $ read ns

parseWhitespace :: Parser String
parseWhitespace = many $ matchIf' isSpace

parseNatAndSpace :: Parser Int
parseNatAndSpace = parseNat >>= \n -> parseWhitespace >> pure n
