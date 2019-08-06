{-# LANGUAGE RecordWildCards #-}

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
module Text.Password.Strength.Internal.Search
  ( Graph(..)
  , Node
  , Edge
  , edges
  , graph
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
import Data.Vector (Vector)

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Estimate
import Text.Password.Strength.Internal.Match
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
-- | A node in a guessing graph.
type Node = Graph.LNode Char

--------------------------------------------------------------------------------
-- | An edge is a guessing graph.
type Edge = Graph.LEdge Int

--------------------------------------------------------------------------------
-- | A password and estimated guesses represented as a graph.
data Graph = Graph
  { exitNode   :: Int
  , graphEdges :: Map (Int, Int) Int
  , scoreGraph :: Gr Char Int
  } deriving Show

--------------------------------------------------------------------------------
-- | Given a password and a user word list, produce graph edges that
-- connect the characters of the password.
edges :: Text -> Vector Text -> Map (Int, Int) Int
edges password = foldr (update . estimate) Map.empty . matches password
  where
    update :: Guesses Match -> Map (Int, Int) Int -> Map (Int, Int) Int
    update guess = let (key, value) = mkEdge guess
                   in Map.insertWith min key value

    mkEdge :: Guesses Match -> ((Int, Int), Int)
    mkEdge (Guesses weight match) =
      let token = match ^. matchToken
          node1 = token ^. startIndex
          node2 = token ^. endIndex
      in ((node1, node2), weight)

--------------------------------------------------------------------------------
-- | Generate a guessing graph from the given password and user word
-- list.  In the guessing graph the nodes are the characters in the
-- password and the edges are the estimated guesses.
graph :: Text -> Vector Text -> Graph
graph password dict =
    Graph exit edges' (Graph.mkGraph nodes (flatten edges'))
  where
    exit :: Int
    exit = Text.length password - 1

    nodes :: [Node]
    nodes = zip [0..] (Text.unpack password)

    edges' :: Map (Int, Int) Int
    edges' = edges password dict

    flatten :: Map (Int, Int) Int -> [(Int, Int, Int)]
    flatten = map (\((x, y), z) -> (x, y, z)) . Map.assocs


--------------------------------------------------------------------------------
-- | Collapse a graph down to a single score which represents the
-- estimated number of guesses it would take to crack the password.
score :: Graph -> Int
score g@Graph{..} =
  case shortestPath g of
    Nothing   -> worstCase
    Just path -> maybe worstCase product (scores (nodes path))

  where
    worstCase :: Int
    worstCase = 10 ^ (exitNode + 1)

    nodes :: [Int] -> [(Int, Int)]
    nodes xs = zip xs (drop 1 xs)

    scores :: [(Int, Int)] -> Maybe [Int]
    scores = mapM (`Map.lookup` graphEdges)

--------------------------------------------------------------------------------
-- | Calculate the shortest path through a guessing graph.  In other
-- words, the cheapest path for guessing a password.
shortestPath :: Graph -> Maybe [Int]
shortestPath Graph{..} = sp 0 exitNode scoreGraph
