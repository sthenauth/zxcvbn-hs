{-# LANGUAGE TemplateHaskell #-}

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

module Text.Password.Strength.SubStr
  ( SubStr(..)
  , allSubstrings
  , substr
  , startIndex
  , endIndex
  , subSplit
  ) where

--------------------------------------------------------------------------------
import Control.Lens.TH (makeLenses)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
data SubStr = SubStr
  { _substr     :: Text
  , _startIndex :: Int
  , _endIndex   :: Int
  } deriving (Show, Eq)

makeLenses ''SubStr

--------------------------------------------------------------------------------
-- | Extract all substrings (with @length >= 3@) from the input 'Text'.
--
-- Examples:
--
-- >>> allSubstrings "abcdef"
-- [SubStr {substr = "abc", startIndex = 0, endIndex = 2},SubStr {substr = "abcd", startIndex = 0, endIndex = 3},SubStr {substr = "abcde", startIndex = 0, endIndex = 4},SubStr {substr = "abcdef", startIndex = 0, endIndex = 5},SubStr {substr = "bcd", startIndex = 1, endIndex = 3},SubStr {substr = "bcde", startIndex = 1, endIndex = 4},SubStr {substr = "bcdef", startIndex = 1, endIndex = 5},SubStr {substr = "cde", startIndex = 2, endIndex = 4},SubStr {substr = "cdef", startIndex = 2, endIndex = 5},SubStr {substr = "def", startIndex = 3, endIndex = 5}]
allSubstrings :: Text -> [SubStr]
allSubstrings = outer 0
  where
    outer :: Int -> Text -> [SubStr]
    outer i t
      | Text.null t = [ ]
      | otherwise   = inner i 2 t ++ outer (i+1) (Text.drop 1 t)

    inner :: Int -> Int -> Text -> [SubStr]
    inner i j t
      | Text.compareLength t (j+1) == LT = [ ]
      | otherwise = SubStr (Text.take (j+1) t) i (i+j) : inner i (j+1) t

--------------------------------------------------------------------------------
newtype SubSplit = SubSplit { unSubSplit :: [Text] }

--------------------------------------------------------------------------------
subSplit :: (Char -> [Char]) -> Text -> [Text]
subSplit f = unSubSplit . Text.foldl split (SubSplit [Text.empty])
  where
    split :: SubSplit -> Char -> SubSplit
    split (SubSplit ts) c =
      case f c of
        [] -> SubSplit (map (`Text.snoc` c) ts)
        xs -> SubSplit (concatMap (\c' -> map (`Text.snoc` c') ts) xs)
