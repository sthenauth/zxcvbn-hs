{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
module Text.Password.Strength.Internal.Config (
  -- * Configuration
  Config,
  HasConfig,
  Dictionary,
  en_US,
  dictionaries,
  passwordLists,
  wordFrequencyLists,
  customFrequencyLists,
  keyboardGraphs,
  obviousSequenceStart,
  addCustomFrequencyList
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((&), (^.), (.~), (%~))
import Control.Lens.TH (makeClassy)
import Control.Monad (join)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

--------------------------------------------------------------------------------
-- Project Imports:
import qualified Text.Password.Strength.Generated.Adjacency as Adjc
import qualified Text.Password.Strength.Generated.Frequency as Freq
import Text.Password.Strength.Internal.Adjacency (AdjacencyTable)

--------------------------------------------------------------------------------
-- | Type alias for a frequency database.
type Dictionary = HashMap Text Int

--------------------------------------------------------------------------------
-- | A type to control which dictionaries, keyboard layouts, etc. will
-- be used when estimating guesses.
data Config = Config
  { _passwordLists :: [Dictionary]
  , _wordFrequencyLists :: [Dictionary]
  , _customFrequencyLists :: [Dictionary]
  , _keyboardGraphs :: [AdjacencyTable]
  , _obviousSequenceStart :: Char -> Bool
  }

makeClassy ''Config

--------------------------------------------------------------------------------
instance Semigroup Config where
  (<>) x y =
      x & passwordLists        %~ (++ (y ^. passwordLists))
        & wordFrequencyLists   %~ (++ (y ^. wordFrequencyLists))
        & customFrequencyLists %~ (++ (y ^. customFrequencyLists))
        & keyboardGraphs       %~ (++ (y ^. keyboardGraphs))
        & obviousSequenceStart .~ oss
      where
        -- Laws:
        --
        -- >>> x <> y
        --
        -- * Left identity:  (\c -> const False c || y c) == y c
        -- * Right identity: (\c -> x c || const False c) == x c
        --
        -- * Associativity:
        --
        --   (\c -> (x c || y c) || z c) == (\c -> (x c || (y c || z c)))
        oss :: Char -> Bool
        oss c = (x ^. obviousSequenceStart) c
             || (y ^. obviousSequenceStart) c

--------------------------------------------------------------------------------
instance Monoid Config where
  mempty = Config [] [] [] [] (const False)

--------------------------------------------------------------------------------
-- | Default configuration for US English.
en_US :: Config
en_US = Config{..}
  where
    _customFrequencyLists = []
    _passwordLists        = [ Freq.xato ]
    _wordFrequencyLists   = [ Freq.english_wikipedia
                            , Freq.female_names
                            , Freq.male_names
                            , Freq.surnames
                            , Freq.us_tv_and_film
                            ]
    _keyboardGraphs       = [ Adjc.qwerty
                            , Adjc.numpad
                            ]
    _obviousSequenceStart c =
      c == 'a' || c == 'A' ||
      c == 'z' || c == 'Z' ||
      c == '0' || c == '1' || c == '9'

--------------------------------------------------------------------------------
-- | Access all configured dictionaries.
dictionaries :: Config -> [Dictionary]
dictionaries c = join [ c ^. passwordLists
                      , c ^. wordFrequencyLists
                      , c ^. customFrequencyLists
                      ]

--------------------------------------------------------------------------------
-- | Add a custom list of words for frequency lookup.  The words
-- should be ordered from most frequent to least frequent.
addCustomFrequencyList :: Vector Text -> Config -> Config
addCustomFrequencyList v = addDict (mkDict v)
  where
    mkDict :: Vector Text -> Dictionary
    mkDict = Vector.ifoldr (\i x -> HashMap.insert x (i+1)) HashMap.empty

    addDict :: Dictionary -> Config -> Config
    addDict d = customFrequencyLists %~ (d:)
