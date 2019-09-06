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
module Text.Password.Strength.Internal.Math
  ( variations
  , variations'
  , bruteForce
  , caps
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((^.))
import Data.Char (isUpper)
import qualified Data.Text as Text
import Numeric.SpecFunctions (choose)

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
-- | Equation 2, section 4, page 163 (8/18)
--
-- NOTE: The other implementations don't seem to divide the sum by two
--       but the equation in the paper clearly does.
variations :: Int -> Int -> Integer
variations 0 _ = 1
variations _ 0 = 1
variations u l = max 1 (sum (floor . choose (u+l) <$> [1 .. min u l]) `div` 2)

--------------------------------------------------------------------------------
-- | Like equation 2, but modified for l33t and keyboard variations.
--
-- This equation does not appear in the 2016 paper although it is
-- mentioned.  Therefore it was adapted from the CoffeeScript and
-- Python implementations.
variations' :: Int -> Int -> Integer
variations' 0 _ = 2
variations' _ 0 = 2
variations' u l = variations u l

--------------------------------------------------------------------------------
-- | Calculate the brute force score for text with the given length.
bruteForce :: Int -> Integer
bruteForce = (10 ^)

--------------------------------------------------------------------------------
-- | Score the use of uppercase letters according to the paper.  This
-- is specified in the paragraph preceding equation 2.
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

--------------------------------------------------------------------------------
-- NOTE: Equation 3 is in Keybaord.hs.
