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
module Text.Password.Strength.Internal.Repeat
  ( RepeatMap
  , Repeat
  , mkRepeatMap
  , repeatMatch
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Arrow ((&&&))
import Control.Lens ((^.), _1)
import Data.Function (on)
import Data.List (sortBy, subsequences, maximumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
-- | Internal mapping of repeating tokens.
newtype RepeatMap = RepeatMap
  { getMap :: Map Text [Token] }

--------------------------------------------------------------------------------
-- | Type alias for a count of repeating tokens.
type Repeat = Int

--------------------------------------------------------------------------------
-- | Generate a repeat map from an existing token map.
mkRepeatMap :: Map Token a -> RepeatMap
mkRepeatMap = RepeatMap . Map.foldrWithKey f Map.empty
  where f t _ = Map.insertWith (<>) (t ^. tokenChars) [t]

--------------------------------------------------------------------------------
-- | Test to see if the given token is repeated.
--
-- If a repeat is found, the number of occurrences is returned along
-- with the full token representing the repeating sequence.
--
-- In other words, if the token passed in is "word" and in the map we
-- find that the original password contains "wordword", we return 2 to
-- indicate 2 repeats and the token that represents the sequence
-- "wordword".
repeatMatch :: RepeatMap -> Token -> Maybe (Repeat, Token)
repeatMatch m t =
    Map.lookup (t ^. tokenChars) (getMap m) >>=
      ordered >>=
        longestSequence >>=
          mkToken
  where
    ordered :: [Token] -> Maybe [Token]
    ordered []  = Nothing
    ordered [_] = Nothing -- Must have at least two elements to repeat.
    ordered xs  = Just $ sortBy (compare `on` (^. startIndex)) xs

    longestSequence :: [Token] -> Maybe (Repeat, [Token])
    longestSequence ts =
      let f = filter (\(n,_) -> n >= 2) .
                map (length &&& id) .
                  filter (all isSequence . lineUp) .
                    subsequences
      in case f ts of
        [] -> Nothing
        xs -> Just $ maximumBy (compare `on` (^. _1)) xs

    mkToken :: (Repeat, [Token]) -> Maybe (Repeat, Token)
    mkToken (_, []) = Nothing
    mkToken (n, ts) = Just $
      let s = head ts ^. startIndex
          e = last ts ^. endIndex
          c = Text.replicate n (t ^. tokenChars)
          l = Text.replicate n (t ^. tokenLower)
      in (n, Token c l s e)

    lineUp :: [Token] -> [(Token, Token)]
    lineUp xs = zip xs (drop 1 xs)

    isSequence :: (Token, Token) -> Bool
    isSequence (x, y) = (y ^. startIndex) - (x ^. endIndex) == 1
