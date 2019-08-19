{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

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
module Text.Password.Strength.Internal.Match
  ( Match(..)
  , Matches
  , matches
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((^.), _1, views, minimumByOf)
import Control.Lens.TH (makePrisms)
import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Config
import Text.Password.Strength.Internal.Dictionary
import Text.Password.Strength.Internal.Keyboard
import Text.Password.Strength.Internal.L33t
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
data Match
  = DictionaryMatch Rank
    -- ^ 'Token' was found in a frequency dictionary with the
    -- specified rank.

  | ReverseDictionaryMatch Rank
    -- ^ 'Token' was found in a frequency dictionary, but only after
    -- the entire password was reversed before splitting into tokens.
    -- The token will be in the original order with the original
    -- coordinates.

  | L33tMatch Rank L33t
    -- ^ 'Token' was found in a frequency dictionary, but only after
    -- the entire password was translated from l33t speak to English.

  | KeyboardMatch KeyboardPattern

  | BruteForceMatch
    -- ^ Used by the scoring system to denote a path that does not
    -- include any of the above matches.

  deriving Show

makePrisms ''Match

--------------------------------------------------------------------------------
-- | Information about how a token matches a specific match pattern.
type Matches = Map Token [Match]

--------------------------------------------------------------------------------
-- | All possible matches after various transformations.
matches :: Config -> Text -> Matches
matches config =
    repeats .
      foldr (\t -> Map.insert t (check t)) Map.empty .
        allTokens
  where
    check :: Token -> [Match]
    check t =
      let ms = catMaybes [ dict t
                         , rdict t
                         , l33ts t
                         ] ++ kbd t
      in case ms of
           [] -> [BruteForceMatch]
           xs -> xs

    -- Tokens that appear in a dictionary.
    dict :: Token -> Maybe Match
    dict t = DictionaryMatch <$> rankFromAll config (^. tokenChars) t

    -- Tokens that, when reversed, appear in a dictionary.
    rdict :: Token -> Maybe Match
    rdict t = ReverseDictionaryMatch <$>
                rankFromAll config (views tokenChars Text.reverse) t

    -- Tokens that, when decoded, appear in a dictionary.
    --
    -- A token may l33t decode into several words that are then looked
    -- up in the word dictionaries.  The word with the lowest rank is
    -- kept and the others are discarded.
    l33ts :: Token -> Maybe Match
    l33ts t =
      let ts = l33t t -- Decoding may result in multiple outputs.
          rnk l = (,l) <$> rankFromAll config (^. l33tText) l
      in uncurry L33tMatch <$>
           minimumByOf traverse (compare `on` (^. _1))
                                (mapMaybe rnk ts)

    -- A token that is a pattern on one or more keyboards.
    kbd :: Token -> [Match]
    kbd t = KeyboardMatch <$>
              mapMaybe (`keyboardPattern` t)
                (config ^. keyboardGraphs)

    -- Tokens that are repeats of previous tokens.
    repeats :: Matches -> Matches
    repeats = id -- FIXME: implement this
