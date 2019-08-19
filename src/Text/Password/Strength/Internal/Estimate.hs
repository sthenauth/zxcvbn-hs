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
module Text.Password.Strength.Internal.Estimate
  ( Guesses
  , Estimates
  , Estimate(..)
  , estimateAll
  , estimate
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((^.))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Project Imports:
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
estimateAll :: Matches -> Guesses
estimateAll ms =
    Map.map (`getEstimate` estimates) estimates
  where
    estimate' :: Token -> [Match] -> Estimates -> Integer
    estimate' t ms' e = sum (map (\m -> estimate t m e) ms')

    estimates :: Estimates
    estimates = Map.mapWithKey (\t m -> Estimate (estimate' t m)) ms

--------------------------------------------------------------------------------
estimate :: Token -> Match -> Estimates -> Integer
estimate token match _ =
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

    BruteForceMatch ->
      let j = token ^. endIndex
          i = token ^. startIndex
      in 10 ^ (j-i+1)
