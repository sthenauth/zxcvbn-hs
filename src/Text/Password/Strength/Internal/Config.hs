{-# LANGUAGE RecordWildCards #-}
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
module Text.Password.Strength.Internal.Config
  ( Config
  , HasConfig
  , Dictionary
  , en_US
  , dictionaries
  , addPasswordDict
  , addWordFrequencyDict
  , addCustomFrequencyList
  , addKeyboardGraph
  , passwordLists
  , wordFrequencyLists
  , customFrequencyLists
  , keyboardGraphs
  , obviousSequenceStart
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
    -- ^ Leaked password frequency lists.

  , _wordFrequencyLists :: [Dictionary]
    -- ^ Various word frequency lists.

  , _customFrequencyLists :: [Dictionary]
    -- ^ Custom word frequency lists.
    --
    -- Usually includes information about the person whose password we
    -- are guessing and the application for which we are running.

  , _keyboardGraphs :: [AdjacencyTable]
    -- ^ Keyboard adjacency graphs.

  , _obviousSequenceStart :: Char -> Bool
    -- ^ Predicate function that should return 'True' for characters
    -- that are "obvious first choices" to start a sequence.  For
    -- example, in English, @a@ and @A@ would be considered 'True'.

  }

makeClassy ''Config

--------------------------------------------------------------------------------
instance Semigroup Config where
  (<>) x y =
      x & passwordLists        %~ (++ (y ^. passwordLists))
        & wordFrequencyLists   %~ (++ (y ^. wordFrequencyLists))
        & customFrequencyLists %~ (++ (y ^. customFrequencyLists))
        & keyboardGraphs       %~ (++ (y ^. keyboardGraphs))
        & obviousSequenceStart .~     (y ^. obviousSequenceStart)

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
-- | Add a password frequency dictionary.
--
-- It's best to generate a dictionary from text data using the tools
-- provided in this package's @tools@ directory.
--
-- If manually generating the dictionary, keep in mind that the @Int@
-- value in the @HashMap@ represents the number of guesses required to
-- crack the password (which is the key of the @HashMap@).  The number of
-- guesses should be @>= 1@.
addPasswordDict :: Dictionary -> Config -> Config
addPasswordDict d = passwordLists %~ (d:)

--------------------------------------------------------------------------------
-- | Add a word frequency dictionary.
--
-- See 'addPasswordDict' for details.
addWordFrequencyDict :: Dictionary -> Config -> Config
addWordFrequencyDict d = wordFrequencyLists %~ (d:)

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

--------------------------------------------------------------------------------
-- | Add a keyboard adjacency graph.
--
-- An adjacency graph can be created with the tools included in this
-- package.  Please see the @tools@ directory for more details.
addKeyboardGraph :: AdjacencyTable -> Config -> Config
addKeyboardGraph g = keyboardGraphs %~ (g:)
