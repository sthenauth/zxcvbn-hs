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
module Text.Password.Strength.Internal.Estimate (
  -- * Estimate Matched Tokens
  Guesses,
  Estimates,
  Estimate(..),
  estimateAll,
  estimate,
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Maybe (fromMaybe)
import Control.Lens ((^.))
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
import Text.Password.Strength.Internal.Sequence
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
-- | Final mapping of a token to its lowest score.
type Guesses = Map Token Integer

--------------------------------------------------------------------------------
-- | Map of partially applied estimates.
type Estimates = Map Token Estimate

--------------------------------------------------------------------------------
-- | A function that will produce an estimate once we know the
-- estimates for other tokens.  This is necessary to score repeat
-- matches since they require looking up the score for a different
-- token.
newtype Estimate = Estimate
  { getEstimate :: Estimates -> Integer }

--------------------------------------------------------------------------------
-- | Estimate all of the given matches.
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
-- | Estimate a single match.
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

    SequenceMatch delta ->
      estimateSequence cfg (token ^. tokenChars) delta

    DateMatch d ->
      estimateDate d

    RepeatMatch n t ->
      let worstcase = bruteForce $ Text.length (token ^. tokenChars)
          guess = (`getEstimate` es) <$> Map.lookup t es
      in fromMaybe worstcase guess * toInteger n
