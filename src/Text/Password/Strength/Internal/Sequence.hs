{-|

Copyright:
  This file is part of the package zxcvbn-hs. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/sthenauth/zxcvbn-hs

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: MIT

-}
module Text.Password.Strength.Internal.Sequence
  ( Delta
  , isSequence
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- | Type alias to represent the distance between characters.
type Delta = Int

--------------------------------------------------------------------------------
-- | If the delta between all of the characters in the given text are
-- the same, that delta is returned.
isSequence :: Text -> Maybe Delta
isSequence t =
  case map measure offset of
    []   -> Nothing
    x:xs -> if all (== x) xs
              then Just x
              else Nothing
  where
    offset :: [(Char, Char)]
    offset = Text.zip t (Text.drop 1 t)

    measure :: (Char, Char) -> Delta
    measure (x, y) = ord y - ord x
