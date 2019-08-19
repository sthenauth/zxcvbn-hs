{-# LANGUAGE OverloadedStrings #-}

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
module Zxcvbn.Repeat
  ( test
  ) where

--------------------------------------------------------------------------------
import Control.Lens
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Text.Password.Strength.Internal

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "Repeat"
  [ tc "ss"       $ \p -> counts p @?= [2]
  , tc "sss"      $ \p -> counts p @?= [2, 3]
  , tc "wordword" $ \p -> counts p @?= [2]
  , tc "abcdefg"  $ \p -> counts p @?= []
  ]

  where
    tc :: Text -> (Text -> Assertion) -> TestTree
    tc t f = testCase (Text.unpack t) (f t)

    counts :: Text -> [Int]
    counts = nub . sort . map (^. _2) . repeatMatch . allTokens
