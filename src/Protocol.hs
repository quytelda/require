{-# LANGUAGE OverloadedStrings #-}

module Protocol where

import           Conduit
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Builder          as B
import           Data.Conduit.Attoparsec
import           Data.List                        (intersperse)
import           Data.Maybe                       (maybeToList)

import           Types

--------------------------------------------------------------------------------
-- Conduits

handshake
  :: MonadThrow m
  => PlayerId
  -> ConduitT ByteString ByteString m ()
handshake pid = do
  lineAsciiC $ sinkParser $ void $ string "HELLO"
  sourceLazy $ B.toLazyByteString $ "HELLO " <> B.intDec pid <> "\n"

parseEvents
  :: Monad m
  => ConduitT ByteString (Either ParseError Event) m ()
parseEvents = peekForever $ lineAsciiC (sinkParserEither parseEvent >>= yield)

renderEvents
  :: Monad m
  => ConduitT Event ByteString m ()
renderEvents = awaitForever $ sourceLazy . B.toLazyByteString . (<> "\n") . renderEvent

--------------------------------------------------------------------------------
-- Parsing

label :: String -> Parser a -> Parser a
label = flip (<?>)

parseTile :: Parser Tile
parseTile = label "parseTile" $
  (,)
  <$> decimal
  <*  char8 '-'
  <*> letter_ascii

parsePlayerId :: Parser PlayerId
parsePlayerId = label "parsePlayerId" decimal

parseCompany :: Parser Company
parseCompany =
  label "parseCompany"
  $   string "Triangle"  *> pure Triangle
  <|> string "Love"      *> pure Love
  <|> string "Armenian"  *> pure Armenian
  <|> string "Fiesta"    *> pure Fiesta
  <|> string "Wonder"    *> pure Wonder
  <|> string "Century"   *> pure Century
  <|> string "Important" *> pure Important

parseEvent :: Parser Event
parseEvent = label "parseEvent" $ do
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
    parseDraw pid = DrawEvent pid
      <$> (string "DRAW" *> optional (space *> parseTile))
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

--------------------------------------------------------------------------------
-- Rendering

renderEvent :: Event -> B.Builder
renderEvent event =
  case event of
    JoinEvent    pid             -> renderEvent' pid "JOIN"
                                    []
    DrawEvent    pid mc          -> renderEvent' pid "DRAW"
                                    (maybeToList $ renderTile <$> mc)
    PlayEvent    pid c           -> renderEvent' pid "PLAY"
                                    [renderTile c]
    DiscardEvent pid c           -> renderEvent' pid "DISCARD"
                                    [renderTile c]
    ReturnEvent  pid c           -> renderEvent' pid "RETURN"
                                    [renderTile c]
    MarkerEvent  pid comp mc     -> renderEvent' pid "MARKER"
                                    $ [renderCompany comp]
                                    <> (maybeToList $ renderTile <$> mc)
    StockEvent   pid comp amount -> renderEvent' pid "STOCK"
                                    [ renderCompany comp
                                    , B.intDec amount
                                    ]
    MoneyEvent pid amount        -> renderEvent' pid "MONEY"
                                    [B.intDec amount]
  where
    renderCompany = B.string8 . show
    renderEvent' pid evt args =
      mconcat $ intersperse (B.char8 ' ') $ B.intDec pid : evt : args

renderTile :: Tile -> B.Builder
renderTile (col, row) = B.intDec col <> B.char8 '-' <> B.char8 row

