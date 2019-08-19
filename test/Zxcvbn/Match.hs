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
module Zxcvbn.Match
  ( test
  ) where

--------------------------------------------------------------------------------
import Control.Lens
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Text.Password.Strength.Internal

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "Match"
  [ shouldRank "password123" "password" 2
  , shouldRank "1drowssap_@" "drowssap" 2
  , shouldRank "/p@ssw0rd^5" "p@ssw0rd" 2
  ]

--------------------------------------------------------------------------------
shouldRank :: Text -> Text -> Int -> TestTree
shouldRank p p' n =
  testCase (Text.unpack p) $ do
    let ms = matches en_US p
        ts = filter (\t -> t ^. tokenChars == p') (Map.keys ms)

    t <- case ts of
           []  -> assertFailure ("missing token: " <> show p' <> " " <> show ms)
           [x] -> pure x
           _   -> assertFailure ("multiple matching tokens! " <> show p')

    case Map.lookup t ms of
      Nothing -> assertFailure "should not happen"
      Just xs -> getRank xs @?= n

  where
    getRank :: [Match] -> Int
    getRank [] = -1
    getRank xs = minimum (map extract xs)

    extract :: Match -> Int
    extract (DictionaryMatch n') = n'
    extract (ReverseDictionaryMatch n') = n'
    extract (L33tMatch n' _) = n'
    extract (KeyboardMatch _) = -1
    extract BruteForceMatch = -1
