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

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Dictionary
import Text.Password.Strength.Internal.Keyboard
import Text.Password.Strength.Internal.L33t
import Text.Password.Strength.Internal.Match
import Text.Password.Strength.Internal.Math
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
data Guesses a = Guesses Integer a
  deriving (Show, Functor)

makePrisms ''Guesses

--------------------------------------------------------------------------------
estimate :: Match -> Guesses Match
estimate match =
  case match of
    DictionaryMatch (Rank n t) ->
      Guesses (caps t (toInteger n)) match

    ReverseDictionaryMatch (Rank n t) ->
      Guesses (caps t (toInteger n * 2)) match

    L33tMatch (Rank n l) ->
      let s = l ^. l33tSub
          u = l ^. l33tUnsub
      in Guesses (toInteger n * variations' s u) match

    KeyboardMatch k ->
      Guesses (keyboardEstimate k) match

    BruteForceMatch t ->
      let j = t ^. endIndex
          i = t ^. startIndex
      in Guesses (10 ^ (j-i+1)) match

  where
    caps :: Token -> Integer -> Integer
    caps token n =
      let text       = token ^. tokenChars
          upper      = Text.length (Text.filter isUpper text)
          lower      = Text.length text - upper
          allLower   = lower == Text.length text
          allUpper   = lower == 0
          firstUpper = upper == 1 && Text.all isUpper (Text.take 1 text)
          lastUpper  = upper == 1 && Text.all isUpper (Text.takeEnd 1 text)
      in case () of
        () | allLower   -> n
           | firstUpper -> n * 2
           | lastUpper  -> n * 2
           | allUpper   -> n * 2
           | otherwise  -> n * variations upper lower
