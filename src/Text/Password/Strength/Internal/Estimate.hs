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
module Text.Password.Strength.Internal.Estimate
  ( Guesses
  , Estimates
  , Estimate(..)
  , estimateAll
  , estimate
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Maybe (fromMaybe)
import Control.Lens ((^.))
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Config
import Text.Password.Strength.Internal.Date
import Text.Password.Strength.Internal.Keyboard
import Text.Password.Strength.Internal.L33t
import Text.Password.Strength.Internal.Match
import Text.Password.Strength.Internal.Math
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
type Estimates = Map Token Estimate

--------------------------------------------------------------------------------
type Guesses = Map Token Integer

--------------------------------------------------------------------------------
newtype Estimate = Estimate
  { getEstimate :: Estimates -> Integer }

--------------------------------------------------------------------------------
estimateAll :: Config -> Matches -> Guesses
estimateAll cfg ms =
    Map.map (`getEstimate` estimates) estimates
  where
    estimate' :: Token -> [Match] -> Maybe (Estimates -> Integer)
    estimate' _ []  = Nothing
    estimate' t ms' = Just (\e -> minimum $ map (\m -> estimate cfg t m e) ms')

    estimates :: Estimates
    estimates =
      let get t m = Estimate <$> estimate' t m
          ins t m tbl = maybe tbl (\e -> Map.insert t e tbl) (get t m)
      in Map.foldrWithKey ins Map.empty ms

--------------------------------------------------------------------------------
estimate :: Config -> Token -> Match -> Estimates -> Integer
estimate cfg token match es =
  case match of
    DictionaryMatch n ->
      caps token (toInteger n)

    ReverseDictionaryMatch n ->
      caps token (toInteger n * 2)

    L33tMatch n l ->
      let s = l ^. l33tSub
          u = l ^. l33tUnsub
      in toInteger n * variations' s u

    KeyboardMatch k ->
      keyboardEstimate k

    RepeatMatch n t ->
      let worstcase = bruteForce $ Text.length (token ^. tokenChars)
          guess = (`getEstimate` es) <$> Map.lookup t es
      in fromMaybe worstcase guess * toInteger n

    SequenceMatch delta ->
      -- Uses the scoring equation from the paper and not from the
      -- other implementations which don't even use the calculated
      -- delta.  The only change from the paper is to compensated for
      -- a delta of 0, which isn't accounted for in the paper.
      let len    = toInteger $ Text.length (token ^. tokenChars)
          start  = if len > 0 then Text.head (token ^. tokenChars) else '\0'
          delta' = toInteger (if delta == 0 then 1 else abs delta)
          base   = case () of
                     () | (cfg ^. obviousSequenceStart) start -> 4
                        | isDigit start                       -> 10
                        | otherwise                           -> 26
      in base * len * delta'

    DateMatch d ->
      estimateDate d
