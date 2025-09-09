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
renderEvents = awaitForever $ sourceLazy . B.toLazyByteString . (<> "\n") . renderEvent

renderErrors :: Monad m => ConduitT ErrorMessage ByteString m ()
renderErrors = awaitForever $ sourceLazy . B.toLazyByteString . (<> "\n") . renderError

renderMessages :: Monad m => ConduitT (Either ErrorMessage Event) ByteString m ()
renderMessages =
  void $ getZipConduit $ (,)
  <$> ZipConduit (lefts .| renderErrors)
  <*> ZipConduit (rights .| renderEvents)
  where
    lefts = awaitForever $ either yield (const $ pure ())
    rights = awaitForever $ either (const $ pure ()) yield

handshake :: MonadThrow m => PlayerId -> ConduitT ByteString ByteString m ()
handshake pid = do
  lineAsciiC $ sinkParser $ void $ string "HELLO"
  sourceLazy $ B.toLazyByteString $ "HELLO " <> B.intDec pid <> "\n"

--------------------------------------------------------------------------------
-- Rendering

renderError :: ErrorMessage -> B.Builder
renderError (ErrorMessage event desc) =
  "ERROR"
  <> B.char8 ' '
  <> quoted (renderEvent event)
  <> B.char8 ' '
  <> B.string8 (show desc)
  where
    quoted b = B.char8 '"' <> b <> B.char8 '"'

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
  where
    renderCompany = B.string8 . show
    renderEvent' pid evt args =
      mconcat $ intersperse (B.char8 ' ') $ B.intDec pid : evt : args

renderCoord :: Coord -> B.Builder
renderCoord (col, row) = B.intDec col <> B.char8 '-' <> B.char8 row

--------------------------------------------------------------------------------
-- Parsing

label :: String -> Parser a -> Parser a
label = flip (<?>)

parseTile :: Parser Coord
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
  $   string "Triangle" *> pure Triangle
  <|> string "Love" *> pure Love
  <|> string "Albanian" *> pure Albanian
  <|> string "Fiesta" *> pure Fiesta
  <|> string "Wonder" *> pure Wonder
  <|> string "Centennial" *> pure Centennial
  <|> string "Imperious" *> pure Imperious

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
