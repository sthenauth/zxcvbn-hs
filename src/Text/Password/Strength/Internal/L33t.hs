{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

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
module Text.Password.Strength.Internal.L33t
  ( L33t
  , l33t
  , l33tToken
  , l33tText
  , l33tSub
  , l33tUnsub
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
data L33t = L33t
  { _l33tToken :: Token
    -- ^ A token containing l33t characters.

  , _l33tText :: Text
    -- ^ The translated (un-l33ted) text.

  , _l33tSub :: Int
    -- ^ Number of substituted l33t characters.

  , _l33tUnsub :: Int
    -- ^ Number of characters in the token that were not substituted
    -- but could have been.

  } deriving Show

makeLenses ''L33t

--------------------------------------------------------------------------------
-- | Translate a token from l33t, counting l33t characters.
l33t :: Token -> [L33t]
l33t = filter hasSubs . map count . trans
  where
    hasSubs :: L33t -> Bool
    hasSubs L33t{..} = _l33tSub > 0 || _l33tUnsub > 0

    chars :: Token -> Text
    chars = (^. tokenChars)

    trans :: Token -> [(Token, Text)]
    trans t = case translateMap l33t2Eng (chars t) of
                [x] | x == (chars t) -> []
                    | otherwise      -> [(t, x)]
                xs                   -> map (t,) xs

    count :: (Token, Text) -> L33t
    count (tk, text) =
      let cnt (x, y) c = (x + l33tCount c, y + engCount c)
          (s, u) = Text.foldl cnt (0, 0) (tk ^. tokenChars)
      in L33t tk text s u

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
