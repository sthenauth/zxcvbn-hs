{-# LANGUAGE RecordWildCards #-}

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
module Text.Password.Strength.Internal.Search
  ( Graph(..)
  , Node
  , Edge
  , edges
  , graph
  , Score(..)
  , score
  , shortestPath
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((^.))
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (sp)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Config
import Text.Password.Strength.Internal.Estimate
import Text.Password.Strength.Internal.Match
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
-- | A node in a guessing graph.
type Node = Graph.LNode Char

--------------------------------------------------------------------------------
-- | An edge is a guessing graph.
type Edge = Graph.LEdge Integer

--------------------------------------------------------------------------------
-- | A password and estimated guesses represented as a graph.
data Graph = Graph
  { exitNode   :: Int
  , graphEdges :: Map (Int, Int) Integer
  , scoreGraph :: Gr Char Integer
  } deriving Show

--------------------------------------------------------------------------------
-- | Given a password and a user word list, produce graph edges that
-- connect the characters of the password.
edges :: Config -> Text -> Map (Int, Int) Integer
edges c p = Map.mapKeys loc (estimateAll c (matches c p))
  where
    -- Convert a token into a location (Node).
    loc :: Token -> (Int, Int)
    loc t = (t ^. startIndex, t ^. endIndex)

--------------------------------------------------------------------------------
-- | Generate a guessing graph from the given password and user word
-- list.  In the guessing graph the nodes are the characters in the
-- password and the edges are the estimated guesses.
graph :: Config -> Text -> Graph
graph config password =
    Graph exit edges' (Graph.mkGraph nodes (flatten edges'))
  where
    exit :: Int
    exit = Text.length password - 1

    nodes :: [Node]
    nodes = zip [0..] (Text.unpack password)

    edges' :: Map (Int, Int) Integer
    edges' = edges config password

    flatten :: Map (Int, Int) Integer -> [(Int, Int, Integer)]
    flatten = map (\((x, y), z) -> (x, y, z)) . Map.assocs

--------------------------------------------------------------------------------
-- | A score is an estimate of the number of guesses it would take to
-- crack a password.
newtype Score = Score { getScore :: Integer }
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- | Collapse a graph down to a single score which represents the
-- estimated number of guesses it would take to crack the password.
score :: Graph -> Score
score g@Graph{..} = Score $
  case shortestPath g of
    Nothing   -> worstCase
    Just path -> maybe worstCase product (scores (nodes path))

  where
    worstCase :: Integer
    worstCase = 10 ^ (exitNode + 1)

    nodes :: [Int] -> [(Int, Int)]
    nodes xs = zip xs (drop 1 xs)

    scores :: [(Int, Int)] -> Maybe [Integer]
    scores = mapM (`Map.lookup` graphEdges)

--------------------------------------------------------------------------------
-- | Calculate the shortest path through a guessing graph.  In other
-- words, the cheapest path for guessing a password.
shortestPath :: Graph -> Maybe [Int]
shortestPath Graph{..} = sp 0 exitNode scoreGraph
