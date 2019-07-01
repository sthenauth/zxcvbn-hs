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
module Text.Password.Strength.Match
  ( Match(..)
  , MatchType(..)
  , matches
  , l33t
  ) where

--------------------------------------------------------------------------------
import Control.Lens (over)
import Control.Monad (join)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.SubStr
import Text.Password.Strength.Dictionary
import qualified Text.Password.Strength.Internal.Frequency as Freq

--------------------------------------------------------------------------------
-- | What type of match was made?
data MatchType = DictionaryMatch
               | ReverseDictionaryMatch
               | L33tMatch
               deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Information about a group of matches.
data Match a = Match MatchType [a]
  deriving Show

--------------------------------------------------------------------------------
-- | All possible matches after various transformations.
matches :: Text -> Vector Text -> [Match (Rank SubStr)]
matches password userDict =
  [ Match DictionaryMatch (dict password)
  , Match ReverseDictionaryMatch rdict
  , Match L33tMatch (l33t password mkUserDict)
  ]
  where
    dict :: Text -> [Rank SubStr]
    dict t = dictionary Text.toLower t mkUserDict

    rdict :: [Rank SubStr]
    rdict = map (fmap (over substr Text.reverse)) (dict $ Text.reverse password)

    mkUserDict :: Dictionary
    mkUserDict = Vector.ifoldr (\i x d -> Map.insert x i d) Map.empty userDict

--------------------------------------------------------------------------------
-- | Substitute l33t characters then run a normal dictionary match.
l33t :: Text -> Dictionary -> [Rank SubStr]
l33t password userDict =
    concatMap (\p -> dictionary Text.toLower p userDict) split
  where
    split :: [Text]
    split = case subSplit dict password of
              [x] | x == password -> []
                  | otherwise     -> [x]
              xs                  -> xs

    dict :: Char -> [Char]
    dict c =
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
-- | Look up all sub-strings in all frequency dictionaries after
-- transforming each sub-string with the given function.
dictionary :: (Text -> Text) -> Text -> Dictionary -> [Rank SubStr]
dictionary f password userDict =
  join [ go Freq.english_wikipedia
       , go Freq.female_names
       , go Freq.male_names
       , go Freq.passwords
       , go Freq.surnames
       , go Freq.us_tv_and_film
       , go userDict
       ]

  where
    go :: Dictionary -> [Rank SubStr]
    go d = rank (f . _substr) d subs

    subs :: [SubStr]
    subs = allSubstrings password
