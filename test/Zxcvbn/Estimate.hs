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
import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Text.Password.Strength.Internal

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "Estimate"
  [ testCase "dict" $
      guess (DictionaryMatch (Rank 2 token)) @?= 2

  , testCase "dict w/ mixed case" $
      guess (DictionaryMatch (Rank 2 uToken)) @?= 8

  , testCase "dict w/ initial upper" $
      guess (DictionaryMatch (Rank 2 cToken)) @?= 4

  , testCase "reverse dict" $
      guess (ReverseDictionaryMatch (Rank 2 token)) @?= 4

  , testCase "l33t" $
      guess (L33tMatch (Rank 2 mkL33t)) @?= 10

  , testCase "brute force" $
      guess (BruteForceMatch token) @?= 100000000
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

    guess :: Match -> Int
    guess m = estimate m ^. _Guesses._1
