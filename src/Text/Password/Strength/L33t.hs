{-# LANGUAGE TupleSections #-}

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
module Text.Password.Strength.L33t
  ( L33tSubbed
  , L33tUnsubbed
  , l33t
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Dictionary
import Text.Password.Strength.Token

--------------------------------------------------------------------------------
-- | Number of l33t characters that were translated to English letters.
type L33tSubbed = Int

--------------------------------------------------------------------------------
-- | Number of English characters that could have been in l33t but were not.
type L33tUnsubbed = Int

--------------------------------------------------------------------------------
-- | A password token and the l33t translated version.
type L33t = (Token, Text)

--------------------------------------------------------------------------------
-- | Substitute l33t characters then run a normal dictionary match.
l33t :: Dictionary -> Text -> [(L33tSubbed, L33tUnsubbed, Rank Token)]
l33t userDict = map (count . fmap fst) . rankFromAll snd userDict . fork
  where
    fork :: Text -> [L33t]
    fork = concatMap trans . allTokens

    trans :: Token -> [(Token, Text)]
    trans t = case translateMap l33t2Eng (_token t) of
                [x] | x == _token t -> []
                    | otherwise     -> [(t, x)]
                xs                  -> map (t,) xs

    count :: Rank Token -> (L33tSubbed, L33tUnsubbed, Rank Token)
    count r@(Rank _ t) =
      let cnt (x, y, z) c = (x + l33tCount c, y + engCount c, z)
      in Text.foldl cnt (0, 0, r) (_token t)

--------------------------------------------------------------------------------
-- | Convert l33t characters to their English character mappings.
l33t2Eng :: Char -> String
l33t2Eng c =
  case c of
    '!' -> ['i']
    '$' -> ['s']
    '%' -> ['x']
    '(' -> ['c']
    '+' -> ['t']
    '0' -> ['o']
    '1' -> ['i', 'l']
    '2' -> ['z']
    '3' -> ['e']
    '4' -> ['a']
    '5' -> ['s']
    '6' -> ['g']
    '7' -> ['l', 't']
    '8' -> ['b']
    '9' -> ['g']
    '<' -> ['c']
    '@' -> ['a']
    '[' -> ['c']
    '{' -> ['c']
    '|' -> ['i', 'l']
    _   -> []

--------------------------------------------------------------------------------
-- | Character count for English letters that can be converted to l33t.
--
-- There's a theoretically possible under estimation here since some
-- English characters can be converted into multiple l33t characters.
-- In reality, English characters tend to have one primary l33t
-- character associated with them.
--
-- An attacker may try all l33t variations for a given English
-- character, but chances are that the primary character was used so
-- we will estimate guesses based on that assumption.
engCount :: Char -> Int
engCount c =
  case c of
    'a' -> 1
    'b' -> 1
    'c' -> 1
    'e' -> 1
    'g' -> 1
    'i' -> 1
    'l' -> 1
    'o' -> 1
    's' -> 1
    't' -> 1
    'x' -> 1
    'z' -> 1
    _   -> 0

--------------------------------------------------------------------------------
-- | Character count for l33t characters that can be converted to l33t.
l33tCount :: Char -> Int
l33tCount c =
  case c of
    '!' -> 1
    '$' -> 1
    '%' -> 1
    '(' -> 1
    '+' -> 1
    '0' -> 1
    '1' -> 1
    '2' -> 1
    '3' -> 1
    '4' -> 1
    '5' -> 1
    '6' -> 1
    '7' -> 1
    '8' -> 1
    '9' -> 1
    '<' -> 1
    '@' -> 1
    '[' -> 1
    '{' -> 1
    '|' -> 1
    _   -> 0
