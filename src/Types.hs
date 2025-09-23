{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Types where


import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.Read   as Read
import           GHC.Generics

--------------------------------------------------------------------------------
-- Basic Game Types

type Money = Int
type PlayerId = Int

-- | Tile coordinates (e.g. "9F"): the column is an integer between 1
-- and 12 (inclusive) and the row is a character between 'A' and 'I'
-- (inclusive).
data Tile = Tile !Int !Char
  deriving (Eq, Ord, Show)

instance ToJSON Tile where
  toJSON (Tile col row) = String $ T.pack $ show col <> [row]

instance FromJSON Tile where
  parseJSON = withText "Tile" $ \text1 -> do
    (col, text2) <- either fail pure $ Read.decimal text1
    (row, text3) <- maybe (fail "missing row") pure $ T.uncons text2

    unless (col >= 1 && col <= 12) $
      fail "column out-of-bounds"

    unless (row >= 'A' && row <= 'I') $
      fail "row out-of-bounds"

    unless (T.null text3) $
      fail $ "unexpected leftover input: " <> show text3

    return $ Tile col row

-- | The set of all possible tiles
allTiles :: [Tile]
allTiles = Tile <$> [1..12] <*> ['A'..'I']

-- | Which game zone is a tile currently in?
data TileLoc
  = Pool          -- ^ The drawing pool
  | Hand PlayerId -- ^ In a player's hand
  | Play          -- ^ On the game board
  | Discard       -- ^ Discarded and unusable
  deriving (Eq, Show)

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

instance ToJSON Company
instance FromJSON Company

-- | Each player and the bank hold a collection of stocks in each
-- company. This type represents a collection of stock assets.
type Stocks = Map Company Int
