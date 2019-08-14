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
module Text.Password.Strength.Internal.Math
  ( variations
  , variations'
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Numeric.SpecFunctions (choose)

--------------------------------------------------------------------------------
-- | Equation 2, section 4, page 163 (8/18)
--
-- NOTE: The other implementations don't seem to divide the sum by two
--       but the equation in the paper clearly does.
variations :: Int -> Int -> Integer
variations 0 _ = 1
variations _ 0 = 1
variations u l = sum (floor . choose (u+l) <$> [1 .. min u l]) `div` 2

--------------------------------------------------------------------------------
-- | Like equation 2, but modified for l33t and keyboard variations.
--
-- This equation does not appear in the 2016 paper although it is
-- mentioned.  Therefore it was adapted from CoffeeScript and Python
-- implementations.
variations' :: Int -> Int -> Integer
variations' 0 _ = 2
variations' _ 0 = 2
variations' u l = variations u l
