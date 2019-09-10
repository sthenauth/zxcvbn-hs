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
module Text.Password.Strength.Internal.Sequence (
  -- * Sequence Matches
  Delta,
  isSequence,
  estimateSequence
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Char (ord, isDigit)
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

--------------------------------------------------------------------------------
-- | Estimate a sequence.
--
-- Uses the scoring equation from the paper and not from the other
-- implementations which don't even use the calculated delta.  The
-- only change from the paper is to compensated for a delta of 0,
-- which isn't accounted for in the paper.
estimateSequence :: (Char -> Bool) -> Text -> Delta -> Integer
estimateSequence f t d =
  let len    = toInteger $ Text.length t
      start  = if len > 0 then Text.head t else '\0'
      delta  = toInteger (if d == 0 then 1 else abs d)
      base   = case () of
                 () | f start       -> 4
                    | isDigit start -> 10
                    | otherwise     -> 26
  in base * len * delta
