{-# LANGUAGE TupleSections #-}

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
module Text.Password.Strength.Internal.Keyboard
  ( KeyboardPattern
  , keyboardToken
  , keyboardPattern
  , keyboardEstimate
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens (Lens, (^.), _3)
import Data.Foldable (foldl')
import Numeric.SpecFunctions (choose)

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Adjacency
import Text.Password.Strength.Internal.Token
import Text.Password.Strength.Internal.Math (variations')

--------------------------------------------------------------------------------
newtype KeyboardPattern =
  KeyboardPattern (Int, Int, Token, AdjacencyScore)
  deriving (Show)

--------------------------------------------------------------------------------
-- | Allow other code to access the token used in a pattern.
keyboardToken :: Lens KeyboardPattern KeyboardPattern Token Token
keyboardToken f (KeyboardPattern t) = KeyboardPattern <$> _3 f t

--------------------------------------------------------------------------------
-- | Helper function to check if a token forms a keyboard pattern.
keyboardPattern :: AdjacencyTable -> Token -> Maybe KeyboardPattern
keyboardPattern graph token = KeyboardPattern .
  (graph ^. totalChars, graph ^. averageNeighbors, token,) .
    foldl' scoreSequence mempty <$> findSequence (token ^. tokenChars) graph

--------------------------------------------------------------------------------
-- | Estimate the number of guesses needed for a keyboard pattern to
-- be cracked.
keyboardEstimate :: KeyboardPattern -> Integer
keyboardEstimate (KeyboardPattern (s, d, _, a)) =
    e3 * e2 (a ^. primaryLayer) (a ^. secondaryLayer)

  where
    -- Equation 3, section 4, page 163 (8/18)
    --
    -- Deviations from the paper or other implementations:
    --
    --   * There's a typo in the paper: min(T, i - i)
    --     but should be: min(T, i - 1)
    --
    --   * Another typo, i should start at 2 and not 1.
    --
    --   * The other implementations don't seem to divide the outer
    --     sum by two but the equation clearly does.
    e3 :: Integer
    e3 = max 1 . (`div` 2) . sum $ do
      i <- [2 .. (a ^. patternLength)]
      j <- [1 .. min (a ^. totalTurns) (i - 1)]
      pure $ floor (choose (i - 1) (j - 1)) * toInteger s * (toInteger d ^ j)

    -- Modified version of equation 2 for primary layer vs. secondary.
    e2 :: Int -> Int -> Integer
    e2 = variations'
