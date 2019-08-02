{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
module Zxcvbn.Match
  ( test
  ) where

--------------------------------------------------------------------------------
import Control.Lens
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Text.Password.Strength

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "Match"
  [ dictTest
  , reverseTest
  , l33tTest
  ]

--------------------------------------------------------------------------------
dictTest :: TestTree
dictTest = testCase (Text.unpack password) $ do
  let m = matches password userDict
      r2 = filter (\(Rank n _) -> n == 2) (rankSort m)

  not (null m)  @? "Non-empty list of matches"
  not (null r2) @? "Should have a rank 2 match"
  "password" @=? (head r2 ^. _Rank._2.tokenChars)

  where
    password :: Text
    password = "password123"

--------------------------------------------------------------------------------
reverseTest :: TestTree
reverseTest = testCase (Text.unpack password) $ do
  let m = matches password userDict
      r2 = filter (\(Rank n _) -> n == 2) (rankSort m)

  not (null m)  @? "Non-empty list of matches"
  not (null r2) @? "Should have a rank 2 match"
  "drowssap" @=? (head r2 ^. _Rank._2.tokenChars)

  where
    password :: Text
    password = "1drowssap_@"

--------------------------------------------------------------------------------
l33tTest :: TestTree
l33tTest = testCase (Text.unpack password) $ do
  let m = matches password userDict
      r = rankSort m

  not (null r)  @? "Non-empty list of ranked matches"
  "p@ssw0rd" @=? (head r ^. _Rank._2.tokenChars)

  where
    password :: Text
    password = "/p@ssw0rd^5"

--------------------------------------------------------------------------------
userDict :: Vector Text
userDict = Vector.singleton "foobar"

--------------------------------------------------------------------------------
matchRank :: Match -> Maybe (Int, Token)
matchRank match =
  case match of
    DictionaryMatch r        -> Just (r ^. _Rank)
    ReverseDictionaryMatch r -> Just (r ^. _Rank)
    L33tMatch  r             -> Just (r ^. _Rank._1, r ^. _Rank._2.l33tToken)
    BruteForceMatch _        -> Nothing

--------------------------------------------------------------------------------
rankSort :: [Match] -> [Rank Token]
rankSort = map (^. re _Rank) . sortBy f . mapMaybe matchRank
  where
    f a b = (a ^. _1) `compare` (b ^. _1)
