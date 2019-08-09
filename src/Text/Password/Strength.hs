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
module Text.Password.Strength
  ( score
  , Search.Score(..)
  , strength
  , Strength(..)
  , Config
  , en_US
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Config
import qualified Text.Password.Strength.Internal.Search as Search

--------------------------------------------------------------------------------
score :: Config -> Text -> Search.Score
score = (Search.score .) . Search.graph

--------------------------------------------------------------------------------
data Strength
  = Risky
    -- ^ Too guessable: risky password. (guesses < 10^3)

  | Weak
    -- ^ Very guessable: protection from throttled online
    -- attacks. (guesses < 10^6)

  | Moderate
    -- ^ Somewhat guessable: protection from unthrottled online
    -- attacks. (guesses < 10^8)

  | Safe
    -- ^ Safely unguessable: moderate protection from offline
    -- slow-hash scenario. (guesses < 10^10)

  | Strong
    -- ^ Very unguessable: strong protection from offline slow-hash
    -- scenario. (guesses >= 10^10)

  deriving (Show, Eq, Ord, Enum)

--------------------------------------------------------------------------------
strength :: Search.Score -> Strength
strength (Search.Score n)
  | n < 10 ^ ( 3 :: Int) = Risky
  | n < 10 ^ ( 6 :: Int) = Weak
  | n < 10 ^ ( 8 :: Int) = Moderate
  | n < 10 ^ (10 :: Int) = Safe
  | otherwise            = Strong
