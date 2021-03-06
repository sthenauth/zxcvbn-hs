{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}

{-|

Copyright:
  This file is part of the package zxcvbn-hs. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/sthenauth/zxcvbn-hs

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: MIT

-}
module Text.Password.Strength.Internal.Match (
  -- * Matching Tokens Against Known Patterns
  Match(..),
  Matches,
  matches
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((^.), _1, views, minimumByOf)
import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Config
import Text.Password.Strength.Internal.Date
import Text.Password.Strength.Internal.Dictionary
import Text.Password.Strength.Internal.Keyboard
import Text.Password.Strength.Internal.L33t
import Text.Password.Strength.Internal.Repeat
import Text.Password.Strength.Internal.Sequence
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
-- | The known patterns we are searching for.
data Match
  = DictionaryMatch Rank
    -- ^ The associated token was found in a frequency dictionary with
    -- the specified rank.

  | ReverseDictionaryMatch Rank
    -- ^ The associated token was found in a frequency dictionary, but
    -- only after its characters were reversed.

  | L33tMatch Rank L33t
    -- ^ The associated token was found in a frequency dictionary, but
    -- only after its characters were translated from l33t speak to
    -- English.

  | KeyboardMatch KeyboardPattern
    -- ^ The associated token is wholly made up of an adjacent
    -- sequence of characters that make a pattern on a keyboard.

  | SequenceMatch Delta
    -- ^ The characters of the associated token form a sequence
    -- because the delta between all the characters is the same.
    --
    -- Examples:
    --
    --   * abc
    ---  * 135

  | DateMatch Date
    -- ^ The associated token wholly contains a date.

  | RepeatMatch Repeat Token
    -- ^ The associated token is an adjacent repeat of another token
    -- (the one given to this constructor).  The number of times it
    -- repeats is given as 'Repeat'.

  deriving Show

--------------------------------------------------------------------------------
-- | Information about how a token matches a specific match pattern.
type Matches = Map Token [Match]

--------------------------------------------------------------------------------
-- | All possible matches after various transformations.
matches :: Config -> Day -> Text -> Matches
matches cfg day =
    repeats .
      foldr (\t -> Map.insert t (check t)) Map.empty .
        allTokens
  where
    check :: Token -> [Match]
    check t = catMaybes
      [dict t, rdict t, l33ts t, seqMatch t, dateMatch t]
      ++ kbd t

    -- Tokens that appear in a dictionary.
    dict :: Token -> Maybe Match
    dict t = DictionaryMatch <$> rank cfg (^. tokenLower) t

    -- Tokens that, when reversed, appear in a dictionary.
    rdict :: Token -> Maybe Match
    rdict t = ReverseDictionaryMatch <$>
                rank cfg (views tokenLower Text.reverse) t

    -- Tokens that, when decoded, appear in a dictionary.
    --
    -- A token may l33t decode into several words that are then looked
    -- up in the word dictionaries.  The word with the lowest rank is
    -- kept and the others are discarded.
    l33ts :: Token -> Maybe Match
    l33ts t =
      let ts = l33t t -- Decoding may result in multiple outputs.
          rnk l = (,l) <$> rank cfg (^. l33tText) l
      in uncurry L33tMatch <$>
           minimumByOf traverse (compare `on` (^. _1))
                                (mapMaybe rnk ts)

    -- A token that is a pattern on one or more keyboards.
    kbd :: Token -> [Match]
    kbd t = KeyboardMatch <$>
              mapMaybe (`keyboardPattern` t)
                (cfg ^. keyboardGraphs)

    -- Characters in a token form a sequence.
    seqMatch :: Token -> Maybe Match
    seqMatch t = SequenceMatch <$> isSequence (t ^. tokenChars)

    -- Characters in a token form a date.
    dateMatch :: Token -> Maybe Match
    dateMatch t = DateMatch <$> isDate day (t ^. tokenChars)

    -- Tokens that are repeats of some other token.
    repeats :: Matches -> Matches
    repeats ms =
      let rmap = mkRepeatMap ms
          f t = (\(n, t') -> (t', [RepeatMatch n t])) <$> repeatMatch rmap t
          g t m = maybe m (\(k,v) -> Map.insertWith (<>) k v m) (f t)
      in Map.foldrWithKey (const . g) ms ms
