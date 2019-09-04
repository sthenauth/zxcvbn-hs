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
module Zxcvbn.Search
  ( test
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Time.Calendar as Time
import Hedgehog hiding (test)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog

--------------------------------------------------------------------------------
import Text.Password.Strength.Internal

--------------------------------------------------------------------------------
-- | Random password generator.
genPassword :: Gen Text
genPassword = Gen.text (Range.linear 1 255) Gen.unicode

--------------------------------------------------------------------------------
prop_has_shortest_path :: Property
prop_has_shortest_path =
  property $ do
    password <- forAll genPassword

    let g = graph en_US ref password
        p = shortestPath g
        ref = Time.fromGregorian 2019 1 1

    annotateShow password
    annotateShow g
    annotateShow p

    isJust p === True

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "Search"
  [ testProperty "shortest path" prop_has_shortest_path
  ]
