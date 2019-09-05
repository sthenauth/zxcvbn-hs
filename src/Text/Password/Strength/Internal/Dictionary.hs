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
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Config

--------------------------------------------------------------------------------
-- | Type to represent a ranking.
type Rank = Int

--------------------------------------------------------------------------------
-- | Look up the given value in all configured dictionaries,
-- transforming each input with the given function.  The lowest ranked
-- score is return if it is found.
rank :: Config -> (a -> Text) -> a -> Maybe Rank
rank c f a =
  case check (dictionaries c) of
    [] -> Nothing
    xs -> Just (minimum xs)
  where
    check :: [Dictionary] -> [Rank]
    check = mapMaybe (HashMap.lookup key)

    key :: Text
    key = f a
