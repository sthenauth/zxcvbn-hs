{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

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
module Zxcvbn.Adjacency
  ( run
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens
import Control.Lens.TH (makePrisms)
import Control.Monad (forM_, join, when)
import Control.Monad.State.Strict
import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath (takeFileName, dropExtension)
import qualified Data.Set as Set
import System.IO

--------------------------------------------------------------------------------
-- Project Imports:
import qualified Text.Password.Strength.Internal as Internal
import Zxcvbn.Encode (header, encode)
import Zxcvbn.Global (Global(..))

import Text.Password.Strength.Internal
  ( Direction(..)
  , Move(..)
  , Adjacency(..)
  , AdjacencyTable(..)
  , Pattern
  )

--------------------------------------------------------------------------------
type Layer  = Int
type Row    = Int
type Column = Int

--------------------------------------------------------------------------------
data Entry
  = A      -- ^ Alignment space.
  | K Char -- ^ Character
  deriving (Show, Eq)

makePrisms ''Entry

--------------------------------------------------------------------------------
data Keyboard = Keyboard
  { kbName   :: String
  , kbLayers :: Int
  , kbTable  :: Map (Layer, Row, Column) Entry
  } deriving (Show)

--------------------------------------------------------------------------------
-- | Translate a line of text into a keyboard row.
parseRow :: Text -> State (Layer, Row) [((Layer, Row, Column), Entry)]
parseRow t | t == "\f" = nextLayer >> pure []
           | otherwise = do
               (layer, row) <- get <* nextRow
               pure $ zipWith (\c n -> ((layer, row, n), classify c))
                              (Text.unpack t) [1..]
  where
    nextLayer :: State (Layer, Row) ()
    nextLayer = do
      _1 += 1
      _2 .= 1

    nextRow :: State (Layer, Row) ()
    nextRow =
      _2 += 1

    classify :: Char -> Entry
    classify c | isSpace c = A
               | otherwise = K c

--------------------------------------------------------------------------------
-- | Parse a keyboard definition file.
keyboard :: FilePath -> Text -> Keyboard
keyboard file t =
  let (rows, (layers, _)) = runState (mapM parseRow $ Text.lines t) (1,1)
  in Keyboard { kbName   = dropExtension (takeFileName file)
              , kbLayers = layers
              , kbTable  = Map.fromList (join rows)
              }

--------------------------------------------------------------------------------
keyboard' :: FilePath -> IO Keyboard
keyboard' f = keyboard f <$> Text.readFile f

--------------------------------------------------------------------------------
-- | Move along the keyboard in the given direction.
move :: Direction -> (Row, Column) -> (Row, Column)
move d (x, y) =
  case d of
    N  -> (x-1, y  )
    NE -> (x-1, y+1)
    E  -> (x,   y+1)
    SE -> (x+1, y+1)
    S  -> (x+1, y  )
    SW -> (x+1, y-1)
    W  -> (x,   y-1)
    NW -> (x-1, y-1)

--------------------------------------------------------------------------------
filterKey :: Keyboard -> Layer -> Direction -> (Row, Column) -> Maybe Char
filterKey k l d (x, y) =
    try =<< Map.lookup (l, x, y) (kbTable k)
  where
    try :: Entry -> Maybe Char
    try (K c) = Just c
    try A     = case d of
                 N  -> Nothing
                 NE -> retry E
                 E  -> retry E
                 SE -> retry E
                 S  -> Nothing
                 SW -> retry W
                 W  -> retry W
                 NW -> retry W

    retry :: Direction -> Maybe Char
    retry d' = filterKey k l d' (move d (x, y))

--------------------------------------------------------------------------------
-- | Calculate all the possible neighbors of a key on a single layer.
-- Does not include the key itself.
neighbors :: (Row, Column) -> [(Direction, (Row, Column))]
neighbors x = map (\d -> (d, move d x)) [minBound .. maxBound]

--------------------------------------------------------------------------------
-- | Generate an adjacency association for the given key.
adjacency :: Keyboard -> (Layer, Row, Column) -> [((Char, Char), Adjacency)]
adjacency k (l, x, y) = fromMaybe [] $ do
    charA <- self l

    pure $ flip concatMap [1 .. kbLayers k] $ \layer ->
      flip map (onLayer layer) $ \(m, charB) ->
        ((charA, charB), Adjacency m (toLayer l) (toLayer layer))

  where
    -- | Find the current character on the given layer.
    self :: Layer -> Maybe Char
    self layer = Map.lookup (layer, x, y) (kbTable k) >>= preview _K

    -- | All adjacent characters on the given layer.
    onLayer :: Layer -> [(Move, Char)]
    onLayer layer = catMaybes $ ((Stay,) <$> self layer) :
      map (\(d, rc) -> (Move d,) <$> filterKey k layer d rc)
          (neighbors (x, y))

    toLayer :: Layer -> Internal.Layer
    toLayer 1 = Internal.Primary
    toLayer _ = Internal.Secondary

--------------------------------------------------------------------------------
-- | Convert a keyboard to an adjacency table.
adjTable :: Keyboard -> AdjacencyTable
adjTable k = AdjacencyTable chars avns table
  where
    -- The adjacency table.
    table :: Map Pattern Adjacency
    table = Map.foldrWithKey append Map.empty (kbTable k)

    -- Add entries to the table.
    append :: (Layer, Row, Column) -> Entry -> Map Pattern Adjacency -> Map Pattern Adjacency
    append key _ m = foldr (uncurry Map.insert) m (adjacency k key)

    -- Total number of keys on the keyboard.
    chars :: Int
    chars = Set.size $ Set.fromList $ map (^. _1._1) $ Map.assocs table

    -- Average number of neighbors.  In the paper, keys can only be
    -- neighbors if they are on the same layer and not the same key.
    -- Also, I truncate the average so we can continue to use integer
    -- arithmetic everywhere.
    avns :: Int
    avns =
      let ns = filter (\a -> _firstLayer a == _secondLayer a) (Map.elems table)
      in (length ns - chars) `div` chars

--------------------------------------------------------------------------------
run :: Global -> IO ()
run Global{..} = do
  putStr (header $ fromMaybe "Text.Password.Strength.Generated.Adjacency" mname)
  putStr "import Text.Password.Strength.Types\n\n"

  forM_ files $ \file -> do
    k@Keyboard{..} <- keyboard' file
    let table = adjTable k

    when debug $ do
      hPutStrLn stderr ("==> " <> file)
      hPutStrLn stderr ("       Total keys: " <> show (_totalChars table))
      hPutStrLn stderr ("Average Neighbors: " <> show (_averageNeighbors table))
      forM_ (Map.assocs (_patterns table)) $ \(key, val) ->
        hPutStrLn stderr (show key <> "\t" <> show val)

    putStrLn (encode kbName "AdjacencyTable" table)
