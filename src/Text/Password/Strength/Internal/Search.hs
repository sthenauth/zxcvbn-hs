{-# LANGUAGE RecordWildCards #-}

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
module Text.Password.Strength.Internal.Search (
  -- * Searching for the Weakest Path Through a Password
  Graph(..),
  Node,
  Edge,
  edges,
  bfEdges,
  graph,
  score,
  shortestPath
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((^.), _1, _2)
import Control.Monad (guard)
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (sp)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Config
import Text.Password.Strength.Internal.Estimate
import Text.Password.Strength.Internal.Match
import Text.Password.Strength.Internal.Math
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
-- | A node in a guessing graph.
type Node = Graph.LNode ()

--------------------------------------------------------------------------------
-- | An edge is a guessing graph.
type Edge = Graph.LEdge Integer

--------------------------------------------------------------------------------
-- | A password and estimated guesses represented as a graph.
data Graph = Graph
  { exitNode   :: Int
  , graphEdges :: Map (Int, Int) Integer
  , scoreGraph :: Gr () Integer
  } deriving Show

--------------------------------------------------------------------------------
-- | Given a password and a user word list, produce graph edges that
-- connect the characters of the password.
edges :: Config -> Day -> Text -> Map (Int, Int) Integer
edges c d p = Map.mapKeys loc (estimateAll c (matches c d p))
  where
    -- Convert a token into a location (Node).
    loc :: Token -> (Int, Int)
    loc t = (t ^. startIndex, t ^. endIndex + 1)

--------------------------------------------------------------------------------
-- | Brute force edges.  In other words, the edges required to ensure
-- there's a path in the graph from the start node to the end node.
bfEdges :: Text -> Map (Int, Int) Integer -> [((Int, Int), Integer)]
bfEdges p es = mapMaybe (fmap guesses . check) rows

  where
    -- A list of rows, where a row is a pairing of a starting point
    -- and all possible ending points.
    rows :: [[(Int, Int)]]
    rows = do
      x  <- 0:map (^. _2) (Map.keys es)
      pure (pair x)

    -- Pair a starting point with an ending point.
    pair :: Int -> [(Int, Int)]
    pair x = do
      y <- map (^. _1) (Map.keys es) ++ [Text.length p]
      guard (y > x)
      pure (x, y)

    -- Check a row to see if an edge needs to be created.  If so, the
    -- required edge is returned.
    check :: [(Int, Int)] -> Maybe (Int, Int)
    check row =
      if any (`Map.member` es) row
        then Nothing
        else listToMaybe row

    guesses :: (Int, Int) -> ((Int, Int), Integer)
    guesses (x, y) = ((x, y), bruteForce (y - x))

--------------------------------------------------------------------------------
-- | Generate a guessing graph from the given password and user word
-- list.  In the guessing graph the nodes are the characters in the
-- password and the edges are the estimated guesses.
graph :: Config -> Day -> Text -> Graph
graph cfg day password =
    Graph exit edges' (Graph.mkGraph nodes (flatten edges'))
  where
    exit :: Int
    exit = Text.length password

    nodes :: [Node]
    nodes = zip [0..exit] (repeat ())

    edges' :: Map (Int, Int) Integer
    edges' =
      let es = edges cfg day password
      in es `Map.union` Map.fromList (bfEdges password es)

    flatten :: Map (Int, Int) Integer -> [(Int, Int, Integer)]
    flatten = map (\((x, y), z) -> (x, y, z)) . Map.assocs

--------------------------------------------------------------------------------
-- | Collapse a graph down to a single score which represents the
-- estimated number of guesses it would take to crack the password.
score :: Graph -> Integer
score g@Graph{..} =
  case shortestPath g of
    Nothing   -> worstCase
    Just path -> maybe worstCase product (scores (nodes path))

  where
    worstCase :: Integer
    worstCase = bruteForce exitNode

    nodes :: [Int] -> [(Int, Int)]
    nodes xs = zip xs (drop 1 xs)

    scores :: [(Int, Int)] -> Maybe [Integer]
    scores = mapM (`Map.lookup` graphEdges)

--------------------------------------------------------------------------------
-- | Calculate the shortest path through a guessing graph.  In other
-- words, the cheapest path for guessing a password.
shortestPath :: Graph -> Maybe [Int]
shortestPath Graph{..} = sp 0 exitNode scoreGraph
