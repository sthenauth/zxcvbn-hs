{-# LANGUAGE TemplateHaskell #-}

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

module Text.Password.Strength.Internal.Token (
    -- * Splitting a Password into Tokens
    Token(..),
    allTokens,

    -- * Lenses for the 'Token' Type
    tokenChars,
    tokenLower,
    startIndex,
    endIndex,

    -- * Translate the Characters of a Password
    translateMap
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens.TH (makeLenses)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- | A token is a substring of a password.
data Token = Token
  { _tokenChars :: Text
  , _tokenLower :: Text
  , _startIndex :: Int
  , _endIndex   :: Int
  } deriving (Show, Eq, Ord)

makeLenses ''Token

--------------------------------------------------------------------------------
-- | Extract all substrings from the input 'Text'.  A substring has a
-- minimum character length of 3 for performance and to prevent false
-- positives for matches such as sequences and repeats.
--
-- Examples:
--
-- >>> map _tokenChars (allTokens "abcdef")
-- ["abc","abcd","abcde","abcdef","bcd","bcde","bcdef","cde","cdef","def"]
allTokens :: Text -> [Token]
allTokens = outer 0
  where
    outer :: Int -> Text -> [Token]
    outer i t
      | Text.null t = [ ]
      | otherwise   = inner i 2 t ++ outer (i+1) (Text.drop 1 t)

    inner :: Int -> Int -> Text -> [Token]
    inner i j t
      | Text.compareLength t (j+1) == LT = [ ]
      | otherwise = mkT i j t : inner i (j+1) t

    mkT :: Int -> Int -> Text -> Token
    mkT i j t =
      let chars = Text.take (j+1) t
      in Token chars (Text.toLower chars) i (i + j)

--------------------------------------------------------------------------------
translateMap :: (Char -> [Char]) -> Text -> [Text]
translateMap f = Text.foldl fork [Text.empty]
  where
    fork :: [Text] -> Char -> [Text]
    fork ts c =
      case f c of
        [] -> map (`Text.snoc` c) ts
        xs -> concatMap (\c' -> map (`Text.snoc` c') ts) xs
