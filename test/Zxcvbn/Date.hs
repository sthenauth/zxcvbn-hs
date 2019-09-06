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
module Zxcvbn.Date
  ( test
  ) where

--------------------------------------------------------------------------------
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time.Calendar as Time
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Text.Password.Strength.Internal

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "Date"
  [ tc "1911"       (@?= Just (2019, 1,  1))
  , tc "19101"      (@?= Just (2019, 10, 1))
  , tc "12151966"   (@?= Just (1966, 12, 15))
  , tc "15121966"   (@?= Just (1966, 12, 15))
  , tc "15021966"   (@?= Just (1966, 2,  15))
  , tc "1999-2-1"   (@?= Just (1999, 2,  1))
  , tc "1999-02-01" (@?= Just (1999, 2,  1))
  ]

  where
    tc :: Text -> (Maybe YMD -> Assertion) -> TestTree
    tc label f = testCase (Text.unpack label) (f (toYMD <$> isDate ref label))

    ref :: Time.Day
    ref = Time.fromGregorian 2019 1 1
