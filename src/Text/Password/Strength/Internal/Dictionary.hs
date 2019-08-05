{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}

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
module Text.Password.Strength.Internal.Dictionary
  ( Dictionary
  , Lookup
  , Rank(..)
  , _Rank
  , rank
  , rankFromAll
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens.TH (makePrisms)
import Control.Monad (join)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Project Imports:
import qualified Text.Password.Strength.Generated.Frequency as Freq

--------------------------------------------------------------------------------
-- | Type alias for a frequency database.
type Dictionary = Map Text Int

--------------------------------------------------------------------------------
type Lookup a = Either a (Rank a)

--------------------------------------------------------------------------------
-- | A ranking for type @a@ based on a frequency database.
data Rank a = Rank Int a deriving (Show, Functor)

makePrisms ''Rank

--------------------------------------------------------------------------------
rankOne :: (a -> Text) -> Dictionary -> a -> Lookup a
rankOne f d a =
  case Rank <$> Map.lookup (Text.toLower $ f a) d <*> pure a of
    Just r  -> Right r
    Nothing -> Left a

--------------------------------------------------------------------------------
-- | Lookup a ranking for all @a@ values and return ranks for those
-- that are in the given frequency database.
rank :: (a -> Text) -> Dictionary -> [a] -> [Lookup a]
rank f d = map (rankOne f d)

--------------------------------------------------------------------------------
-- | Look up all inputs in all frequency dictionaries after
-- transforming each input with the given function.
rankFromAll :: (a -> Text) -> Dictionary -> [a] -> [Lookup a]
rankFromAll f userDict as =
  let go dict = rank f dict as
  in join [ go Freq.english_wikipedia
          , go Freq.female_names
          , go Freq.male_names
          , go Freq.passwords
          , go Freq.surnames
          , go Freq.us_tv_and_film
          , go userDict
          ]
