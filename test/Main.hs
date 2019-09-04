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
module Main (main) where

--------------------------------------------------------------------------------
import Test.Tasty

--------------------------------------------------------------------------------
import qualified Zxcvbn.Adjacency
import qualified Zxcvbn.Date
import qualified Zxcvbn.Estimate
import qualified Zxcvbn.Match
import qualified Zxcvbn.Search
import qualified Zxcvbn.Repeat
import qualified Zxcvbn.Sequence

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "zxcbn"
  [ Zxcvbn.Match.test
  , Zxcvbn.Estimate.test
  , Zxcvbn.Search.test
  , Zxcvbn.Adjacency.test
  , Zxcvbn.Repeat.test
  , Zxcvbn.Sequence.test
  , Zxcvbn.Date.test
  ]
