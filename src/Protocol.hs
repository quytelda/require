{-# LANGUAGE OverloadedStrings #-}

module Protocol where

import           Conduit
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Builder          as B
import           Data.Conduit.Attoparsec
import           Data.List                        (intersperse)
import           Data.Maybe                       (maybeToList)

import           Types

--------------------------------------------------------------------------------
-- Conduits

parseEvents :: MonadThrow m => ConduitT ByteString Event m ()
parseEvents = peekForever $ lineAsciiC ((sinkParser parseEvent) >>= yield)

renderEvents :: Monad m => ConduitT Event ByteString m ()
renderEvents = awaitForever $ sourceLazy . B.toLazyByteString . renderEvent

handshake :: MonadThrow m => PlayerId -> ConduitT ByteString ByteString m ()
handshake pid = do
  lineAsciiC $ sinkParser $ void $ string "HELLO"
  sourceLazy $ B.toLazyByteString $ "HELLO " <> B.intDec pid <> "\n"

--------------------------------------------------------------------------------
-- Rendering

renderEvent :: Event -> B.Builder
renderEvent e =
  case e of
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
    <> "\n"
  where
    renderCompany = B.string8 . show
    renderEvent' pid evt args =
      mconcat $ intersperse (B.char8 ' ') $ B.intDec pid : evt : args

renderCoord :: Coord -> B.Builder
renderCoord (col, row) = B.intDec col <> B.char8 '-' <> B.char8 row

--------------------------------------------------------------------------------
-- Parsing

parseTile :: Parser Coord
parseTile =
  (,)
  <$> decimal
  <*  char8 '-'
  <*> letter_ascii

parsePlayerId :: Parser PlayerId
parsePlayerId = decimal <?> "PlayerId"

parseCompany :: Parser Company
parseCompany =
  string "Triangle" *> pure Triangle
  <|> string "Love" *> pure Love
  <|> string "Albanian" *> pure Albanian
  <|> string "Fiesta" *> pure Fiesta
  <|> string "Wonder" *> pure Wonder
  <|> string "Centennial" *> pure Centennial
  <|> string "Imperious" *> pure Imperious

parseEvent :: Parser Event
parseEvent = do
  pid <- parsePlayerId
  void space

  parseJoin pid
    <|> parseDraw pid
    <|> parsePlay pid
    <|> parseDiscard pid
    <|> parseReturn pid
    <|> parseMarker pid
    <|> parseMoney pid
    <|> parseStock pid
  where
    parseJoin pid = string "JOIN"
      *> pure (JoinEvent pid)
    parseDraw pid = string "DRAW"
      *> (DrawEvent pid <$> optional (space *> parseTile))
    parsePlay pid = PlayEvent pid
      <$> (string "PLAY" *> space *> parseTile)
    parseDiscard pid = DiscardEvent pid
      <$> (string "DISCARD" *> space *> parseTile)
    parseReturn pid = ReturnEvent pid
      <$> (string "RETURN" *> space *> parseTile)
    parseMarker pid = MarkerEvent pid
      <$> (string "MARKER" *> space *> parseCompany)
      <*> optional (space *> parseTile)
    parseMoney pid = MoneyEvent pid
      <$> (string "MONEY" *> space *> signed decimal)
    parseStock pid = StockEvent pid
      <$> (string "STOCK" *> space *> parseCompany)
      <* space
      <*> signed decimal
