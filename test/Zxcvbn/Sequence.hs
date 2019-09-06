{-# LANGUAGE OverloadedStrings #-}

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
module Zxcvbn.Sequence
  ( test
  ) where

--------------------------------------------------------------------------------
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Text.Password.Strength.Internal

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "Sequence"
  [ tc "abcdef" (@?= Just 1)
  , tc "123456" (@?= Just 1)
  , tc "9753"   (@?= Just (-2))
  , tc "acbrf"  (@?= Nothing)
  , tc "aaaaaa" (@?= Just 0)
  , tc "λνορσυ" (@?= Just 2)
  ]

  where
    tc :: Text -> (Maybe Delta -> Assertion) -> TestTree
    tc label f = testCase (Text.unpack label) (f $ isSequence label)
