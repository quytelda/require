{-# LANGUAGE OverloadedStrings #-}

module Protocol where

import           Conduit
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.Conduit.Attoparsec

import           Types

--------------------------------------------------------------------------------
-- Conduits

parseEvents :: MonadThrow m => ConduitT ByteString Event m ()
parseEvents = conduitParser (event <* (endOfLine <|> endOfInput)) .| mapC snd

--------------------------------------------------------------------------------
-- Parsers

lexeme :: Parser ByteString
lexeme = takeTill isSpace

boundary :: Parser ()
boundary = space *> skipSpace

quotedString :: Parser ByteString
quotedString = do
  void $ char '"'
  str <- takeTill (== '"')
  void $ char '"'
  return str

company :: Parser Company
company = string "Triangle" *> pure Triangle
          <|> string "Love" *> pure Love
          <|> string "Albanian" *> pure Albanian
          <|> string "Fiesta" *> pure Fiesta
          <|> string "Wonder" *> pure Wonder
          <|> string "Centennial" *> pure Centennial
          <|> string "Imperious" *> pure Imperious

coord :: Parser Coord
coord = do
  col <- decimal
  void $ char '-'
  row <- satisfy (\c -> c >= 'A' && c <= 'I')
  return (col, row)

event :: Parser Event
event = do
  pid <- decimal
  boundary
  action <- lexeme
  skipSpace

  case action of
    "NAME"    -> NameEvent pid <$> quotedString
    "DRAW"    -> DrawEvent pid <$> optional coord
    "PLAY"    -> PlayEvent pid <$> coord
    "DISCARD" -> DiscardEvent pid <$> coord
    "RETURN"  -> ReturnEvent pid <$> coord
    "MARKER"  -> MarkerEvent pid <$> optional coord
    "MONEY"   -> MoneyEvent pid <$> signed decimal
    "STOCK"   -> StockEvent pid <$> company <* space <*> signed decimal
    _         -> undefined -- TODO: Error handling
