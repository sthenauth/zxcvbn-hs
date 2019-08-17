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
module Zxcvbn.Adjacency
  ( test
  ) where

--------------------------------------------------------------------------------
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Text.Password.Strength.Internal

--------------------------------------------------------------------------------
simpleGraph :: AdjacencyTable
simpleGraph =  AdjacencyTable
    { _totalChars = 5
    , _averageNeighbors = 3
    , _patterns = makeDict
    }
  where
    makeDict :: Map Pattern Adjacency
    makeDict = Map.fromList [ (('a', 's'), Adjacency (Move E)  Primary   Primary)
                            , (('s', 'E'), Adjacency (Move NE) Primary   Secondary)
                            , (('e', 'w'), Adjacency (Move W)  Primary   Primary)
                            , (('E', 'w'), Adjacency (Move W)  Secondary Primary)
                            , (('w', 'a'), Adjacency (Move SW) Primary   Primary)
                            ]

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "Adjacency"
  [ testCase "asE" $ adj "asE" @?= Just (AdjacencyScore 3 2 2 1 (Move NE))
  , testCase "sEw" $ adj "sEw" @?= Just (AdjacencyScore 3 2 2 1 (Move W))
  , testCase "ewa" $ adj "ewa" @?= Just (AdjacencyScore 3 2 3 0 (Move SW))
  , testCase "aSE" $ adj "aSE" @?= Nothing
  ]

  where
    adj :: Text -> Maybe AdjacencyScore
    adj p = foldl' scoreSequence mempty <$> findSequence p simpleGraph
