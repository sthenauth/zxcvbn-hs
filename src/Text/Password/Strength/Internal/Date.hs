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
module Text.Password.Strength.Internal.Date (
  -- * Date Matches
  Date,
  YMD,
  isDate,
  toYMD,
  estimateDate
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((&), (^.), (+~), _1)
import Control.Lens.TH (makeLenses)
import qualified Data.Attoparsec.Text as Atto
import Data.Char (isDigit, isSpace)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Time.Calendar as Time

--------------------------------------------------------------------------------
-- | A date as a triple.
data Date = Date
  { _year    :: Int     -- ^ A recent year.
  , _month   :: Int     -- ^ 1-12.
  , _day     :: Int     -- ^ 1-31.
  , _hasSep  :: Bool    -- ^ Was a separator found in the date string?
  , _refYear :: Integer -- ^ What year are we comparing to?
  } deriving Show

makeLenses ''Date

--------------------------------------------------------------------------------
-- | Components of a found date (year, month, day).
type YMD = (Int, Int, Int)

--------------------------------------------------------------------------------
-- | Helper function to construct a 'Date' record.
toDate :: Bool -> Integer -> YMD -> Date
toDate s r (x,y,z) = Date x y z s r

--------------------------------------------------------------------------------
-- | Extract the date components of a 'Date' record.
toYMD :: Date -> YMD
toYMD d = (d ^. year, d ^. month, d ^. day)

--------------------------------------------------------------------------------
-- | If the given text wholly contains a date, return it.
isDate :: Time.Day -> Text -> Maybe Date
isDate ref t =
  listToMaybe $
    order $
      filter valid $
        map fixYear dates
  where
    dates :: [Date]
    dates =
      case Atto.parseOnly dateAvecSep t of
        Left _   -> toDate False refY <$> dateSansSep t
        Right ds -> toDate True  refY <$> ds

    order :: [Date] -> [Date]
    order = sortBy (compare `on` distance)

    distance :: Date -> Integer
    distance d = abs (toInteger (d ^. year) - refY)

    valid :: Date -> Bool
    valid date =
      let d = date ^. day
          m = date ^. month
          y = date ^. year
      in    m >= 1 && m <= 12
         && d >= 1 && d <= 31
         && y >= lastCentury
         && y <= (thisCentury + 100)

    fixYear :: Date -> Date
    fixYear d | (d ^. year) > 99 = d
              | (d ^. year) > 50 = d & year +~ lastCentury
              | otherwise        = d & year +~ thisCentury

    -- Reference year for sorting and scoring.
    refY :: Integer
    refY = Time.toGregorian ref ^. _1

    lastCentury :: Int
    lastCentury = fromInteger ((refY `div` 100) - 1) * 100

    thisCentury :: Int
    thisCentury = fromInteger refY `div` 100 * 100

--------------------------------------------------------------------------------
-- | Estimate the number of guesses for a date match.
--
-- Deviations from the zxcvbn paper:
--
--   1. The other implementations limit the year multiplier to 20 so
--      we do the same here.
--
--   2. The other implementations multiply by 4 when date separators
--      are used in the token.  We do the same.
estimateDate :: Date -> Integer
estimateDate d =
  let space = max (abs (toInteger (d ^. year) - (d ^. refYear))) 20
      guesses = max 1 space * 365
  in if d ^. hasSep
       then guesses * 4
       else guesses

--------------------------------------------------------------------------------
-- | Helper type for a triple of @Text -> Maybe a@ parser.
type Read3 a = (Maybe a, Maybe a, Maybe a)

--------------------------------------------------------------------------------
-- | A function that can rearrange a triple.
type Arrange a = Read3 a -> Read3 a

--------------------------------------------------------------------------------
-- | Extract all possible date combinations from the given text.
dateSansSep :: Text -> [YMD]
dateSansSep t
  | not (Text.all isDigit t) = []
  | otherwise = catMaybes
  [ take3 (1, 1, 2) dmy
  , take3 (2, 1, 1) ymd
  , take3 (2, 2, 0) ym_
  , take3 (2, 2, 0) my_
  , take3 (1, 2, 2) dmy
  , take3 (2, 1, 2) mdy
  , take3 (2, 2, 1) ymd
  , take3 (2, 1, 2) ymd
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
    take3 :: (Int, Int, Int) -> Arrange Int -> Maybe YMD
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
    seq3 :: Read3 Int -> Maybe YMD
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
dateAvecSep :: Atto.Parser [YMD]
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
