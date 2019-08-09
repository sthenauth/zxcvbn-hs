{-# LANGUAGE DeriveFunctor   #-}
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
import Control.Lens ((^.))
import Control.Lens.TH (makePrisms)
import Control.Monad (join)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Config

--------------------------------------------------------------------------------
-- | Type to indicate whether or not a value can be ranked.
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
rank :: (a -> Text) -> [a] -> Dictionary -> [Lookup a]
rank f xs d = map (rankOne f d) xs

--------------------------------------------------------------------------------
-- | Look up all inputs in all frequency dictionaries after
-- transforming each input with the given function.
rankFromAll :: Config -> (a -> Text) -> [a] -> [Lookup a]
rankFromAll c f as =
  let go = concatMap (rank f as)
  in join [ go (c ^. passwordLists)
          , go (c ^. wordFrequencyLists)
          , go (c ^. customFrequencyLists)
          ]
