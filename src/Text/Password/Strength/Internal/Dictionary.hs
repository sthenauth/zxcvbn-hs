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
module Text.Password.Strength.Internal.Dictionary
  ( Dictionary
  , Rank
  , rank
  , rankFromAll
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((^.))
import Control.Monad (join)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Config

--------------------------------------------------------------------------------
-- | Type to represent a ranking.
type Rank = Int

--------------------------------------------------------------------------------
-- | Look up the given value in a given dictionary.
rank :: (a -> Text) -> a -> Dictionary -> Maybe Rank
rank f a = Map.lookup (Text.toLower $ f a)

--------------------------------------------------------------------------------
-- | Look up the given value in all configured dictionaries,
-- transforming each input with the given function.  The lowest ranked
-- score is return if it is found.
rankFromAll :: Config -> (a -> Text) -> a -> Maybe Rank
rankFromAll c f a =
  case check dicts of
    [] -> Nothing
    xs -> Just (minimum xs)
  where
    check :: [Dictionary] -> [Rank]
    check = mapMaybe (rank f a)

    dicts :: [Dictionary]
    dicts = join [ c ^. passwordLists
                 , c ^. wordFrequencyLists
                 , c ^. customFrequencyLists
                 ]
