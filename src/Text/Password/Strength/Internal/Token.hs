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

module Text.Password.Strength.Internal.Token (
    -- * Splitting a Password into Tokens
    Token(..),
    allTokens,

    -- * Lenses for the 'Token' Type
    tokenChars,
    startIndex,
    endIndex,

    -- * Translate the Characters of a Password
    translateMap
  ) where

--------------------------------------------------------------------------------
import Control.Lens.TH (makeLenses)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
data Token = Token
  { _tokenChars :: Text
  , _startIndex :: Int
  , _endIndex   :: Int
  } deriving (Show, Eq, Ord)

makeLenses ''Token

--------------------------------------------------------------------------------
-- | Extract all substrings from the input 'Text'.
--
-- Examples:
--
-- >>> allTokens "abcdef"
--
allTokens :: Text -> [Token]
allTokens = outer 0
  where
    outer :: Int -> Text -> [Token]
    outer i t
      | Text.null t = [ ]
      | otherwise   = inner i 1 t ++ outer (i+1) (Text.drop 1 t)

    inner :: Int -> Int -> Text -> [Token]
    inner i j t
      | Text.compareLength t (j+1) == LT = [ ]
      | otherwise = Token (Text.take (j+1) t) i (i+j) : inner i (j+1) t

--------------------------------------------------------------------------------
translateMap :: (Char -> [Char]) -> Text -> [Text]
translateMap f = Text.foldl fork [Text.empty]
  where
    fork :: [Text] -> Char -> [Text]
    fork ts c =
      case f c of
        [] -> map (`Text.snoc` c) ts
        xs -> concatMap (\c' -> map (`Text.snoc` c') ts) xs
