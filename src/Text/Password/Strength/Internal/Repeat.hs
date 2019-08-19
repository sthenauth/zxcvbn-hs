{-# LANGUAGE TupleSections #-}

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
module Text.Password.Strength.Internal.Repeat
  ( repeatMatch
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
-- | Length of text if all characters are the same.
repeatingChar :: Text -> Maybe Int
repeatingChar t | Text.length t == 0 = Nothing
                | otherwise          = go
  where
    go :: Maybe Int
    go = if Text.all (== Text.head t) t
           then Just (Text.length t)
           else Nothing

--------------------------------------------------------------------------------
duplicateToken :: (Foldable t) => Token -> t Token -> Maybe Int
duplicateToken t ts = if count /= 0 then Just (count + 1) else Nothing
  where
    count :: Int
    count = foldr match 0 ts

    match :: Token -> Int -> Int
    match t' n =
      n + (if t' /= t && (t' ^. tokenChars) == (t ^. tokenChars)
             then 1
             else 0)

--------------------------------------------------------------------------------
repeatMatch :: (Foldable t) => t Token -> [(Token, Int)]
repeatMatch ts = catMaybes (foldr match [] ts)
  where
    match :: Token -> [Maybe (Token, Int)] -> [Maybe (Token, Int)]
    match t rs = ((t,) <$> check t):rs

    check :: Token -> Maybe Int
    check t = repeatingChar (t ^. tokenChars) <|> duplicateToken t ts
