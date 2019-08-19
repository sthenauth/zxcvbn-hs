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
module Zxcvbn.Estimate
  ( test
  ) where

--------------------------------------------------------------------------------
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Text.Password.Strength.Internal

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "Estimate"
  [ testCase "dict" $
      guess token (DictionaryMatch 2) @?= 2

  , testCase "dict w/ mixed case" $
      guess uToken (DictionaryMatch 2) @?= 8

  , testCase "dict w/ initial upper" $
      guess cToken (DictionaryMatch 2) @?= 4

  , testCase "reverse dict" $
      guess token (ReverseDictionaryMatch 2) @?= 4

  , testCase "l33t" $
      guess token (L33tMatch 2 mkL33t) @?= 10

  , testCase "brute force" $
      guess token BruteForceMatch @?= 100000000
  ]

  where
    token :: Token
    token = Token "password" 0 7

    uToken :: Token
    uToken = Token "passWord" 0 7

    cToken :: Token
    cToken = Token "Password" 0 7

    mkL33t :: L33t
    mkL33t = head (l33t $ Token "p@ssw0rd" 0 7)

    guess :: Token -> Match -> Integer
    guess t m = estimate t m Map.empty
