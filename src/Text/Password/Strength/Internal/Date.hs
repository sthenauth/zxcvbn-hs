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
module Text.Password.Strength.Internal.Date
  ( Year
  , Month
  , Day
  , Date
  , isDate
  ) where

--------------------------------------------------------------------------------
import Control.Lens ((^.), _1)
import qualified Data.Attoparsec.Text as Atto
import Data.Char (isSpace)
import Data.List (sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Data.Function (on)
import qualified Data.Time.Calendar as Time

--------------------------------------------------------------------------------
-- | Type alias for a recent year.
type Year = Int

--------------------------------------------------------------------------------
-- | Type alias for a month (1..12).
type Month = Int

--------------------------------------------------------------------------------
-- | Type alias for a day (1..31).
type Day = Int

--------------------------------------------------------------------------------
-- | A date as a triple.
type Date = (Year, Month, Day)

--------------------------------------------------------------------------------
-- | If the given text wholly contains a date, return it.
isDate :: Time.Day -> Text -> Maybe Date
isDate day t =
  listToMaybe $
    order $
      filter valid $
        map fixYear dates
  where
    dates :: [Date]
    dates =
      case Atto.parseOnly dateAvecSep t of
        Left _   -> dateSansSep t
        Right ds -> ds

    order :: [Date] -> [Date]
    order = sortBy (compare `on` distance)

    distance :: Date -> Integer
    distance (y,_,_) = abs (toInteger y - refYear)

    valid :: Date -> Bool
    valid (y,m,d) = m >= 1 && m <= 12
                 && d >= 1 && d <= 31
                 && y >= lastCentury
                 && y <= (thisCentury + 100)

    fixYear :: Date -> Date
    fixYear (y,m,d) | y > 99    = (y, m, d)
                    | y > 50    = (lastCentury + y, m, d)
                    | otherwise = (thisCentury + y, m, d)

    -- Reference year for sorting and scoring.
    refYear :: Integer
    refYear = Time.toGregorian day ^. _1

    lastCentury :: Int
    lastCentury = fromInteger ((refYear `div` 100) - 1) * 100

    thisCentury :: Int
    thisCentury = fromInteger refYear `div` 100 * 100

--------------------------------------------------------------------------------
-- | Helper type for a triple of @Text -> Maybe a@ parser.
type Read3 a = (Maybe a, Maybe a, Maybe a)

--------------------------------------------------------------------------------
-- | A function that can rearrange a triple.
type Arrange a = Read3 a -> Read3 a

--------------------------------------------------------------------------------
-- | Extract all possible date combinations from the given text.
dateSansSep :: Text -> [Date]
dateSansSep t = catMaybes
  [ take3 (1, 1, 2) dmy
  , take3 (2, 1, 1) ymd
  , take3 (2, 2, 0) ym_
  , take3 (2, 2, 0) my_
  , take3 (1, 2, 2) dmy
  , take3 (2, 1, 2) mdy
  , take3 (1, 1, 4) dmy
  , take3 (1, 1, 4) mdy
  , take3 (2, 2, 2) dmy
  , take3 (2, 2, 2) mdy
  , take3 (2, 2, 2) ymd
  , take3 (4, 1, 1) ymd
  , take3 (1, 2, 4) dmy
  , take3 (1, 2, 4) mdy
  , take3 (2, 1, 4) dmy
  , take3 (2, 1, 4) mdy
  , take3 (4, 1, 2) ymd
  , take3 (4, 2, 1) ymd
  , take3 (2, 2, 4) dmy
  , take3 (2, 2, 4) mdy
  , take3 (4, 2, 2) ymd
  ]

  where
    -- Parse three numbers and reorder them.
    take3 :: (Int, Int, Int) -> Arrange Int -> Maybe Date
    take3 (x,y,z) f
      | (x+y+z) /= Text.length t = Nothing
      | otherwise =
        let g = seq3 . f . read3
        in g ( Text.take x t
             , Text.take y (Text.drop x t)
             , Text.drop (x+y) t
             )

    -- Parser.
    read3 :: (Text, Text, Text) -> Read3 Int
    read3 (x, y, z) =
      let r = either (const Nothing) check . Text.decimal
          check (n,e) | Text.null e = Just n
                      | otherwise   = Nothing
      in (r x, r y, r z)

    -- Sequence for a triple.
    seq3 :: Read3 Int -> Maybe Date
    seq3 (x, y, z) = (,,) <$> x <*> y <*> z

    -- Arrangement functions.
    dmy (d,m,y) = (y,m,d)
    mdy (m,d,y) = (y,m,d)
    ym_ (y,m,_) = (y,m, pure 1)
    my_ (m,y,_) = (y,m, pure 1)
    ymd         = id

--------------------------------------------------------------------------------
-- | Extract all possible date combinations that include component
-- separators.
dateAvecSep :: Atto.Parser [Date]
dateAvecSep = do
    ds1 <- Atto.decimal
    sep <- Atto.satisfy isSep
    ds2 <- Atto.decimal
    _   <- Atto.char sep
    ds3 <- Atto.decimal
    Atto.endOfInput

    pure [ (ds1, ds2, ds3) -- Y-M-D
         , (ds3, ds2, ds1) -- D-M-Y
         , (ds3, ds1, ds2) -- M-D-Y
         ]
  where
    isSep :: Char -> Bool
    isSep c = isSpace c ||
              c == '/'  ||
              c == '\\' ||
              c == '.'  ||
              c == '_'  ||
              c == '-'
