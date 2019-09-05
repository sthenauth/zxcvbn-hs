{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Text.Password.Strength.Internal

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "Repeat"
  [ tc "wordword"      "word" $ Just (2, Token "wordword" "wordword" 0 7)
  , tc "wordword@word" "word" $ Just (2, Token "wordword" "wordword" 0 7)
  , tc "word@wordword" "word" $ Just (2, Token "wordword" "wordword" 5 12)
  , tc "word2word"     "word" Nothing
  , tc "word"          "word" Nothing
  , tc "abcdefg"       "abc"  Nothing
  ]

  where
    tc :: Text -> Text -> Maybe (Int, Token) -> TestTree
    tc p t r = testCase (Text.unpack p) $
      repeatMatch (rmap p) (Token t (Text.toLower t) 0 0) @?= r

    tmap :: Text -> Map Token ()
    tmap = Map.fromList . map (,()) . allTokens

    rmap :: Text -> RepeatMap
    rmap = mkRepeatMap . tmap
