{-# LANGUAGE OverloadedStrings #-}

module Protocol where

import           Conduit
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.ByteString.Builder          as B
import           Data.Conduit.Attoparsec
import           Data.List                        (intersperse)
import           Data.Maybe                       (maybeToList)

import           Types

--------------------------------------------------------------------------------
-- Conduits

parseEvents :: MonadThrow m => ConduitT ByteString Event m ()
parseEvents = conduitParser (event <* (endOfLine <|> endOfInput)) .| mapC snd

--------------------------------------------------------------------------------
-- Rendering

renderEvent :: Event -> B.Builder
renderEvent e =
  case e of
    HelloEvent   pid             -> renderEvent' 0 "HELLO"
                                    [B.intDec pid]
    JoinEvent    pid             -> renderEvent' pid "JOIN"
                                    []
    DrawEvent    pid mc          -> renderEvent' pid "DRAW"
                                    (maybeToList $ renderCoord <$> mc)
    PlayEvent    pid c           -> renderEvent' pid "PLAY"
                                    [renderCoord c]
    DiscardEvent pid c           -> renderEvent' pid "DISCARD"
                                    [renderCoord c]
    ReturnEvent  pid c           -> renderEvent' pid "RETURN"
                                    [renderCoord c]
    MarkerEvent  pid comp mc     -> renderEvent' pid "MARKER"
                                    $ [renderCompany comp]
                                    <> (maybeToList $ renderCoord <$> mc)
    StockEvent   pid comp amount -> renderEvent' pid "STOCK"
                                    [ renderCompany comp
                                    , B.intDec amount
                                    ]
    MoneyEvent pid amount        -> renderEvent' pid "MONEY"
                                    [B.intDec amount]
    _                            -> undefined
  where
    renderCompany = B.string8 . show
    renderEvent' pid evt args =
      mconcat $ intersperse (B.char8 ' ') $ B.intDec pid : evt : args

renderCoord :: Coord -> B.Builder
renderCoord (col, row) = B.intDec col <> B.char8 '-' <> B.char8 row

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
    "HELLO"   -> HelloEvent <$> decimal
    "JOIN"    -> pure $ JoinEvent pid
    "NAME"    -> NameEvent pid <$> quotedString
    "DRAW"    -> DrawEvent pid <$> optional coord
    "PLAY"    -> PlayEvent pid <$> coord
    "DISCARD" -> DiscardEvent pid <$> coord
    "RETURN"  -> ReturnEvent pid <$> coord
    "MARKER"  -> MarkerEvent pid <$> company <*> optional (space *> coord)
    "MONEY"   -> MoneyEvent pid <$> signed decimal
    "STOCK"   -> StockEvent pid <$> company <* space <*> signed decimal
    _         -> undefined -- TODO: Error handling
