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
  , addPasswordDict
  , addWordFrequencyDict
  , addCustomFrequencyList
  , passwordLists
  , wordFrequencyLists
  , customFrequencyLists
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((&), (^.), (%~))
import Control.Lens.TH (makeClassy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

--------------------------------------------------------------------------------
-- Project Imports:
import qualified Text.Password.Strength.Generated.Frequency as Freq

--------------------------------------------------------------------------------
-- | Type alias for a frequency database.
type Dictionary = Map Text Int

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
  }

makeClassy ''Config

--------------------------------------------------------------------------------
instance Semigroup Config where
  (<>) x y =
      x & passwordLists        %~ (++ (y ^. passwordLists))
        & wordFrequencyLists   %~ (++ (y ^. wordFrequencyLists))
        & customFrequencyLists %~ (++ (y ^. customFrequencyLists))

--------------------------------------------------------------------------------
instance Monoid Config where
  mempty = Config [] [] []

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

--------------------------------------------------------------------------------
-- | Add a password frequency dictionary.
--
-- It's best to generate a dictionary from text data using the tools
-- provided in this package's @tools@ directory.
--
-- If manually generating the dictionary, keep in mind that the @Int@
-- value in the @Map@ represents the number of guesses required to
-- crack the password (which is the key of the @Map@).  The number of
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
    mkDict = Vector.ifoldr (\i x -> Map.insert x (i+1)) Map.empty

    addDict :: Dictionary -> Config -> Config
    addDict d = customFrequencyLists %~ (d:)
