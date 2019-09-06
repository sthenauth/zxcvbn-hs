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

This is a native Haskell implementation of
the [zxcvbn](https://github.com/dropbox/zxcvbn) password strength
estimation algorithm as it appears in the 2016 USENIX
Security [paper and presentation](https://www.usenix.org/conference/usenixsecurity16/technical-sessions/presentation/wheeler)
(with some small modifications).

-}
module Text.Password.Strength (
  -- * Estimating Guesses
  score,
  Search.Score(..),

  -- * Calculating Password Strength
  strength,
  Strength(..),

  -- * Default Configuration
  en_US

  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Text (Text)
import Data.Time.Calendar (Day)

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Config
import qualified Text.Password.Strength.Internal.Search as Search

--------------------------------------------------------------------------------
-- | Estimate the number of guesses an attacker would need to make to
-- crack the given password.
score :: Config -- ^ Which dictionaries, keyboards, etc. to use.
      -> Day    -- ^ Reference day for date matches (should be current day).
      -> Text   -- ^ The password to score.
      -> Search.Score -- ^ Estimate.
score c d p = Search.score (Search.graph c d p)

--------------------------------------------------------------------------------
-- | Measurement of password strength.
data Strength
  = Risky
    -- ^ Too guessable: risky password. (guesses < \(10^{3}\))

  | Weak
    -- ^ Very guessable: protection from throttled online
    -- attacks. (guesses < \(10^{6}\))

  | Moderate
    -- ^ Somewhat guessable: protection from unthrottled online
    -- attacks. (guesses < \(10^{8}\))

  | Safe
    -- ^ Safely unguessable: moderate protection from offline
    -- slow-hash scenario. (guesses < \(10^{10}\))

  | Strong
    -- ^ Very unguessable: strong protection from offline slow-hash
    -- scenario. (guesses >= \(10^{10}\))

  deriving (Show, Read, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- | Calculate the strength of a password given its score.
strength :: Search.Score -> Strength
strength (Search.Score n)
  | n < 10 ^ ( 3 :: Int) = Risky
  | n < 10 ^ ( 6 :: Int) = Weak
  | n < 10 ^ ( 8 :: Int) = Moderate
  | n < 10 ^ (10 :: Int) = Safe
  | otherwise            = Strong
