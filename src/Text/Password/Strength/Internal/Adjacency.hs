{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Copyright:
  This file is part of the package zxcvbn-hs. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/open/zxcvbn-hs

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: MIT

-}
module Text.Password.Strength.Internal.Adjacency
  ( Pattern
  , Direction(..)
  , Layer(..)
  , Adjacency(..)
  , AdjacencyTable(..)
  , totalChars
  , averageNeighbors
  , patterns
  , findSequence
  , AdjacencyScore(..)
  , patternLength
  , totalTurns
  , primaryLayer
  , secondaryLayer
  , scoreSequence
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (+~), (?~))
import Control.Lens.TH (makeLenses)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- | A @Pattern@ is two Unicode characters next to one another in a password.
type Pattern = (Char, Char)

--------------------------------------------------------------------------------
data Direction = N | NE | E | SE | S | SW | W | NW
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
data Layer = Primary | Secondary
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- | Information about how two characters are related to one another.
data Adjacency = Adjacency
  { _direction :: Direction
    -- ^ The direction moving from the first to second character.

  , _firstLayer :: Layer
    -- ^ The layer that the first character is on.

  , _secondLayer :: Layer
    -- ^ The layer that the second character is on.
  }
  deriving (Show)

makeLenses ''Adjacency

--------------------------------------------------------------------------------
-- | An adjacency graph (usually representing a single keyboard).
data AdjacencyTable = AdjacencyTable
  { _totalChars :: Int
    -- ^ Total number of characters in the graph (total keys on the
    -- keyboard including all layers).

  , _averageNeighbors :: Int
    -- ^ Average number of neighbors in the graph.

  , _patterns :: Map Pattern Adjacency
    -- ^ Dictionary for looking up patterns.

  } deriving (Show)

makeLenses ''AdjacencyTable

--------------------------------------------------------------------------------
-- | Find a pattern if it exists.  If all characters in the given
-- 'Text' form a pattern in the given 'Graph' then a list of matches
-- will be returned.
findSequence :: Text -> AdjacencyTable -> Maybe (NonEmpty Adjacency)
findSequence t AdjacencyTable{..} =
  let chars = Text.unpack t
      ms = mapM (`Map.lookup` _patterns) (zip chars (drop 1 chars))
  in NonEmpty.fromList <$> ms

--------------------------------------------------------------------------------
-- | Scoring information for adjacent characters.
data AdjacencyScore = AdjacencyScore
  { _patternLength :: Int
    -- ^ Number of characters in the pattern.

  , _totalTurns :: Int
    -- ^ Total number of turns needed.

  , _primaryLayer :: Int
    -- ^ Characters that are on the primary layer.

  , _secondaryLayer :: Int
    -- ^ Characters that are on a secondary layer.

  , _lastDirection :: Maybe Direction
    -- ^ The direction on the last character.

  } deriving (Show, Eq)

makeLenses ''AdjacencyScore

--------------------------------------------------------------------------------
instance Semigroup AdjacencyScore where
  (<>) (AdjacencyScore l t p s d) (AdjacencyScore l' t' p' s' d') =
    AdjacencyScore (l+l') (t+t') (p+p') (s+s') (d <|> d')

--------------------------------------------------------------------------------
instance Monoid AdjacencyScore where
  mempty = AdjacencyScore 0 0 0 0 Nothing

--------------------------------------------------------------------------------
-- | Calculate the score for two adjacent characters.
scoreSequence :: AdjacencyScore -> Adjacency -> AdjacencyScore
scoreSequence s a =
  s & turns
    & layers
    & patternLength +~ (if (s ^. patternLength) == 0 then 2 else 1)
    & lastDirection ?~ (a ^. direction)

  where
    turns :: AdjacencyScore -> AdjacencyScore
    turns = if Just (a ^. direction) /= (s ^. lastDirection)
              then totalTurns +~ 1
              else id

    -- Usually we focus on the layer of the second character but when
    -- we are looking at the start of the pattern we need to consider
    -- both characters.
    layers :: AdjacencyScore -> AdjacencyScore
    layers = if (s ^. patternLength) == 0
               then layer (a ^. firstLayer) . (& layer (a ^. secondLayer))
               else layer (a ^. secondLayer)

    layer :: Layer -> AdjacencyScore -> AdjacencyScore
    layer Primary   = primaryLayer   +~ 1
    layer Secondary = secondaryLayer +~ 1
