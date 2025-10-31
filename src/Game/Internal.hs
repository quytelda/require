{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Game.Internal where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import qualified Data.Text.Read             as Read
import           GHC.Generics
import           Servant

type Money = Int
type PlayerId = Int

-- | Tile coordinates (e.g. "9F"): the column is an integer between 1
-- and 12 (inclusive) and the row is a character between 'A' and 'I'
-- (inclusive).
data Tile = Tile !Int !Char
  deriving (Eq, Ord, Show)

renderTile :: Tile -> TB.Builder
renderTile (Tile col row) = TBI.decimal col <> TB.singleton row

parseTile :: Text -> Either String Tile
parseTile text1 = do
  (col, text2) <- Read.decimal text1
  (row, text3) <- maybe (Left "missing row") Right $ T.uncons text2

  unless (col >= 1 && col <= 12) $
    Left "column out-of-bounds"

  unless (row >= 'A' && row <= 'I') $
    Left "row out-of-bounds"

  unless (T.null text3) $
    Left $ "unexpected leftover input: " <> show text3

  return $ Tile col row

instance ToJSON Tile where
  toJSON = textBuilderToJSON . renderTile

instance FromJSON Tile where
  parseJSON = withText "Tile" $ either fail pure . parseTile

instance ToJSONKey Tile where
  toJSONKey = toJSONKeyText (TL.toStrict . TB.toLazyText . renderTile)

instance FromHttpApiData Tile where
  parseQueryParam = first T.pack . parseTile

--------------------------------------------------------------------------------
-- Utility Functions

textBuilderToJSON :: TB.Builder -> Value
textBuilderToJSON = String . TL.toStrict . TB.toLazyText

-- | Utility function to print a Text builder.
--
-- We need to convert the builder to strict Text and append the
-- newline before rendering to avoid avoid interleaved output when
-- printing from different threads.
putBuilderLn :: TB.Builder -> IO ()
putBuilderLn =
  TIO.putStr
  . TL.toStrict
  . TB.toLazyText
  . (<> TB.singleton '\n')

-- | The set of all possible tiles
allTiles :: [Tile]
allTiles = Tile <$> [1..12] <*> ['A'..'I']

-- | Which game zone is a tile currently in?
data TileZone
  = Pool          -- ^ The drawing pool
  | Hand PlayerId -- ^ In a player's hand
  | Play          -- ^ On the game board
  | Discard       -- ^ Discarded and unusable
  deriving (Eq, Show)

renderTileZone :: TileZone -> TB.Builder
renderTileZone Pool       = "pool"
renderTileZone (Hand pid) = "hand/" <> TBI.decimal pid
renderTileZone Play       = "play"
renderTileZone Discard    = "discard"

parseTileZone :: Text -> Either String TileZone
parseTileZone "pool"    = pure Pool
parseTileZone "play"    = pure Play
parseTileZone "discard" = pure Discard
parseTileZone txt =
  fmap (Hand . fst)
  . maybe (Left "invalid tile zone") Read.decimal
  . T.stripPrefix "hand/"
  $ txt

instance ToJSON TileZone where
  toJSON = textBuilderToJSON . renderTileZone

instance FromJSON TileZone where
  parseJSON = withText "TileZone" $ either fail pure . parseTileZone

instance FromHttpApiData TileZone where
  parseQueryParam = first T.pack . parseTileZone

-- | Companies in which players may invest
data Company
  = Triangle
  | Love
  | Armenian
  | Fiesta
  | Wonder
  | Century
  | Important
  deriving (Enum, Eq, Show, Ord, Generic)

renderCompany :: Company -> TB.Builder
renderCompany Triangle  = "Triangle"
renderCompany Love      = "Love"
renderCompany Armenian  = "Armenian"
renderCompany Fiesta    = "Fiesta"
renderCompany Wonder    = "Wonder"
renderCompany Century   = "Century"
renderCompany Important = "Important"

instance ToJSON Company where
  toJSON = textBuilderToJSON . renderCompany

instance FromJSON Company

instance ToJSONKey Company where
  toJSONKey = toJSONKeyText (TL.toStrict . TB.toLazyText . renderCompany)

instance FromHttpApiData Company where
  parseQueryParam "Triangle"  = Right Triangle
  parseQueryParam "Love"      = Right Love
  parseQueryParam "Armenian"  = Right Armenian
  parseQueryParam "Fiesta"    = Right Fiesta
  parseQueryParam "Wonder"    = Right Wonder
  parseQueryParam "Century"   = Right Century
  parseQueryParam "Important" = Right Important
  parseQueryParam _           = Left "invalid company name"
