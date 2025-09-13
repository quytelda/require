{-# LANGUAGE OverloadedStrings #-}

module Protocol
  ( handshake
  , parseEvents
  , renderEvents
  , renderErrors
  , renderEvent
  , renderError
  , printEvent
  , printError
  ) where

import           Conduit
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Builder          as B
import           Data.Conduit.Attoparsec
import           Data.Functor
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
  => PlayerId
  -> ConduitT ByteString (Either ParseError Event) m ()
parseEvents pid =
  peekForever $ lineAsciiC (sinkParserEither (parseEvent pid) >>= yield)

renderEvents
  :: Monad m
  => ConduitT Event ByteString m ()
renderEvents = awaitForever $ sourceLazy . B.toLazyByteString . (<> "\n") . renderEvent

renderErrors
  :: Monad m
  => ConduitT RequireException ByteString m ()
renderErrors = awaitForever $ yield . toStrictByteString . (<> "\n") . renderError

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

parseCompany :: Parser Company
parseCompany =
  label "parseCompany"
  $   string "Triangle"  $> Triangle
  <|> string "Love"      $> Love
  <|> string "Armenian"  $> Armenian
  <|> string "Fiesta"    $> Fiesta
  <|> string "Wonder"    $> Wonder
  <|> string "Century"   $> Century
  <|> string "Important" $> Important

parseEvent :: PlayerId -> Parser Event
parseEvent pid = label "parseEvent" $
  parseJoin
  <|> parseDraw
  <|> parsePlay
  <|> parseDiscard
  <|> parseReturn
  <|> parseMarker
  <|> parseMoney
  <|> parseStock
  where
    parseJoin =
      JoinEvent pid
      <$ string "JOIN"
    parseDraw =
      DrawEvent pid
      <$ string "DRAW"
      <*> optional (space *> parseTile)
    parsePlay =
      PlayEvent pid
      <$ string "PLAY"
      <* space
      <*> parseTile
    parseDiscard =
      DiscardEvent pid
      <$ string "DISCARD"
      <* space
      <*> parseTile
    parseReturn =
      ReturnEvent pid
      <$ string "RETURN"
      <* space
      <*> parseTile
    parseMarker =
      MarkerEvent pid
      <$ string "MARKER"
      <* space
      <*> parseCompany
      <*> optional (space *> parseTile)
    parseMoney =
      MoneyEvent pid
      <$ string "MONEY"
      <* space
      <*> signed decimal
    parseStock =
      StockEvent pid
      <$ string "STOCK"
      <* space
      <*> parseCompany
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
                                    <> maybeToList (renderTile <$> mc)
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

renderError :: RequireException -> B.Builder
renderError (ParseException _ err)   = renderParseError err
renderError (EventException event err) = renderEventException event err

renderParseError :: ParseError -> B.Builder
renderParseError err =
  "PARSE_ERROR \""
  <> B.string8 (show err)
  <> B.char8 '"'

renderEventException :: Event -> GameError -> B.Builder
renderEventException event err =
  "EVENT_ERROR \""
  <> renderEvent event
  <> "\" "
  <> B.string8 (show $ displayException err)

--------------------------------------------------------------------------------
-- Utility Functions

toStrictByteString :: B.Builder -> ByteString
toStrictByteString = BS.toStrict . B.toLazyByteString

printEvent :: MonadIO m => Event -> m ()
printEvent event = liftIO
  $ putBuilder
  $ "> "
  <> renderEvent event
  <> B.char8 '\n'

printError :: MonadIO m => RequireException -> m ()
printError err = liftIO
  $ errBuilder
  $ "error [PID "
  <> B.intDec (errorSource err)
  <> "]: "
  <> B.string8 (displayException err)
  <> B.char8 '\n'
