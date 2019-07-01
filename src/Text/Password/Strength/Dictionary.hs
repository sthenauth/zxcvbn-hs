{-# LANGUAGE DeriveFunctor #-}

{-|

Copyright:
  This file is part of the package sthenauth. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/open/zxcvbn-hs

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: MIT

-}
module Text.Password.Strength.Dictionary
  ( Dictionary
  , Rank(..)
  , rank
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

--------------------------------------------------------------------------------
-- | Type alias for a frequency database.
type Dictionary = Map Text Int

--------------------------------------------------------------------------------
-- | A ranking for type @a@ based on a frequency database.
data Rank a = Rank Int a deriving (Show, Functor)

--------------------------------------------------------------------------------
-- | Lookup a ranking for all @a@ values and return ranks for those
-- that are in the given frequency database.
rank :: (a -> Text) -> Dictionary -> [a] -> [Rank a]
rank f d = mapMaybe (\x -> Rank <$> Map.lookup (f x) d <*> pure x)
