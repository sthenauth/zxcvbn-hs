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
module Text.Password.Strength.Internal.Estimate
  ( Guesses(..)
  , _Guesses
  , estimate
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((^.))
import Control.Lens.TH (makePrisms)
import Data.Char (isUpper)
import qualified Data.Text as Text
import Numeric.SpecFunctions (choose)

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Dictionary
import Text.Password.Strength.Internal.L33t
import Text.Password.Strength.Internal.Match
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
data Guesses a = Guesses Int a
  deriving (Show, Functor)

makePrisms ''Guesses

--------------------------------------------------------------------------------
estimate :: Match -> Guesses Match
estimate match =
  case match of
    DictionaryMatch (Rank n t) ->
      Guesses (caps t n) match

    ReverseDictionaryMatch (Rank n t) ->
      Guesses (caps t (n*2)) match

    L33tMatch (Rank n l) ->
      let s = l ^. l33tSub
          u = l ^. l33tUnsub
      in Guesses (n * l33tV s u) match

    BruteForceMatch t ->
      let j = t ^. endIndex
          i = t ^. startIndex
      in Guesses (10 ^ (j-i+1)) match

  where
    caps :: Token -> Int -> Int
    caps token n =
      let text = token ^. tokenChars
          upper = Text.length (Text.filter isUpper text)
          lower = Text.length text - upper
          allLower = lower == Text.length text
          allUpper = lower == 0
          firstUpper = upper == 1 && Text.all isUpper (Text.take 1 text)
          lastUpper  = upper == 1 && Text.all isUpper (Text.takeEnd 1 text)
      in case () of
        () | allLower   -> n
           | firstUpper -> n * 2
           | lastUpper  -> n * 2
           | allUpper   -> n * 2
           | otherwise  -> n * variations upper lower

    l33tV :: Int -> Int -> Int
    l33tV 0 _ = 2 -- No substitute characters
    l33tV _ 0 = 2 -- All characters substituted
    l33tV s u = variations s u

    variations :: Int -> Int -> Int
    variations u l =
      let range = [1 .. min u l]
          ul    = u + l
          sigma = sum (map (ul `choose`) range)
       in floor (sigma / 2)
