{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
module Main (main) where

--------------------------------------------------------------------------------
-- Imports:
import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize (encode)
import Data.Serialize.Text ()
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Pipes (Pipe, (>->), runEffect, each)
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Prelude.Text as PT
import Pipes.Safe (runSafeT)
import System.Environment (getArgs)
import System.FilePath (takeFileName, dropExtension)

--------------------------------------------------------------------------------
-- | A list that we'll read from a file.
data List = List
  { path  :: FilePath
  , name  :: Text
  , limit :: Maybe Int
  } deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- | Words read from all frequency lists.
data Entry = Entry
  { list :: List  -- ^ The list this entry was read from.
  , rank :: Int   -- ^ The rank this word has in the given list
  } deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- | The temporary map used to hold all frequency lists.
type Freqs = Map Text Entry

--------------------------------------------------------------------------------
-- | Insert an entry into the map.  If the entry is already in the
-- table keep the one with the lowest rank.
insert :: (Text, Entry) -> Freqs -> Freqs
insert (word, ent) = Map.insertWith go word ent
  where
    go :: Entry -> Entry -> Entry
    go new old = if rank new < rank old then new else old

--------------------------------------------------------------------------------
-- | Process the lines from the given list, adding entries to 'Freqs'.
processList :: List -> Freqs -> IO Freqs
processList lst tbl = runSafeT $ runEffect $
    Pipes.fold (\b e -> insert (entry e) b) tbl id
      (Pipes.zip (each [1..])
        (PT.readFileLn (path lst) >->
         Pipes.map word           >->
         restrict) >-> Pipes.filter (not . shouldThrowAway))

  where
    -- Create an entry with the given rank and line of text.
    entry :: (Int, Text) -> (Text, Entry)
    entry (n, t) = (t, Entry lst n)

    -- Extract the first word from a line of text.
    word :: Text -> Text
    word = Text.takeWhile (not . isSpace)

    shouldThrowAway :: (Int, Text) -> Bool
    shouldThrowAway (n, t) = n >= 10 ^ Text.length t

    -- Only process @limit@ lines of text.
    restrict :: (Monad m) => Pipe a a m ()
    restrict =
      case limit lst of
        Just n  -> Pipes.take n
        Nothing -> Pipes.drop 0

--------------------------------------------------------------------------------
-- | Alias for a map whose keys are list names and values are a map of
-- words and ranks.
type Lists = Map Text (Map Text Int)

--------------------------------------------------------------------------------
-- | Given a @Freqs@ table, extract all the component lists.
extractLists :: Freqs -> Lists
extractLists = Map.foldrWithKey go Map.empty
  where
    go :: Text -> Entry -> Lists -> Lists
    go word Entry{..} = ins (name list) word rank

    ins :: Text -> Text -> Int -> Lists -> Lists
    ins lst word rnk = Map.insertWith Map.union lst (Map.singleton word rnk)

--------------------------------------------------------------------------------
-- | Given a list and its ranked entries, serialize to Haskell.
encodeList :: Text -> Map Text Int -> String
encodeList lst tbl =
  mconcat [ Text.unpack lst
          , " :: Map Text Int\n"
          , Text.unpack lst
          , " = "
          , "either undefined id $ decode "
          , show (encode tbl)
          ]

--------------------------------------------------------------------------------
-- | Turn a file name with an optional line number limit into a 'List'.
mkList :: String -> List
mkList s =
  case Text.split (== ':') (Text.pack s) of
    [x,y] -> List (Text.unpack x) (name' x) (limit' y)
    _     -> List s (name' $ Text.pack s) Nothing

  where
    name' :: Text -> Text
    name' = Text.pack . dropExtension . takeFileName . Text.unpack

    limit' :: Text -> Maybe Int
    limit' t = case Text.decimal t of
      Left _       -> Nothing
      Right (n, _) -> Just n

--------------------------------------------------------------------------------
-- | Let's go!
main :: IO ()
main = do
  ls <- extractLists <$> (foldM (flip processList) Map.empty =<<
                           (map mkList <$> getArgs))

  putStrLn $ mconcat [ "-- This file is automatically generated, DO NOT EDIT!\n"
                     , "{-# LANGUAGE OverloadedStrings #-}\n"
                     , "module Text.Password.Strength.Generated.Frequency ("
                     , Text.unpack $ Text.intercalate ", " (Map.keys ls)
                     , ") where\n"
                     , "import Data.Serialize (decode)\n"
                     , "import Data.Serialize.Text ()\n"
                     , "import Data.Text (Text)\n"
                     , "import Data.Map.Strict (Map)\n"
                     ]

  mapM_ (putStrLn . uncurry encodeList) (Map.assocs ls)
