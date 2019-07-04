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
  , matches
  , l33t
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((&), (^.), (%~), (.~))
import Control.Monad (join)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Dictionary
import Text.Password.Strength.L33t
import Text.Password.Strength.Token

--------------------------------------------------------------------------------
data Match
  = DictionaryMatch (Rank Token)
    -- ^ 'Token' was found in a frequency dictionary with the
    -- specified rank.

  | ReverseDictionaryMatch (Rank Token)
    -- ^ 'Token' was found in a frequency dictionary, but only after
    -- the entire password was reversed before splitting into tokens.
    -- The token will be in the original order with the original
    -- coordinates.

  | L33tMatch L33tSubbed L33tUnsubbed (Rank Token)
    -- ^ 'Token' was found in a frequency dictionary, but only after
    -- the entire password was translated from l33t speak to English.
    --
    -- The meaning of 'L33tSubbed' and 'L33tUnsubbed' can be found in
    -- their respective type definitions.

  | BruteForceMatch
    -- ^ Used by the scoring system to denote a path that does not
    -- include any of the above matches.

  deriving Show

--------------------------------------------------------------------------------
-- | All possible matches after various transformations.
matches :: Text -> Vector Text -> [Match]
matches password userVec =
  join [ DictionaryMatch <$> dict password
       , ReverseDictionaryMatch <$> rdict
       , l33tMatch password
       ]
  where
    dict :: Text -> [Rank Token]
    dict = rankFromAll _token userDict . allTokens

    rdict :: [Rank Token]
    rdict = map (fmap unReverse) (dict $ Text.reverse password)

    -- Fix a token that was generated from @rdict@.
    unReverse :: Token -> Token
    unReverse t = let lastI = Text.length password - 1
                  in t & token      %~ Text.reverse
                       & startIndex .~ (lastI - (t ^. endIndex))
                       & endIndex   .~ (lastI - (t ^. startIndex))

    l33tMatch :: Text -> [Match]
    l33tMatch = map (\(x,y,z) -> L33tMatch x y z) . l33t userDict

    -- Generate a user dictionary from a 'Vector'.
    userDict :: Dictionary
    userDict = Vector.ifoldr (\i x d -> Map.insert x (i+1) d) Map.empty userVec
