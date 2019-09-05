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
module Zxcvbn.Freq
  ( run
  ) where

--------------------------------------------------------------------------------
-- Imports:
import Control.Monad (forM_, foldM)
import Data.Char (isSpace)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Pipes (Pipe, (>->), runEffect, each)
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Prelude.Text as PT
import Pipes.Safe (runSafeT)
import System.FilePath (takeFileName, dropExtension)

--------------------------------------------------------------------------------
import Zxcvbn.Encode (header, encode)
import Zxcvbn.Global (Global(..))

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
type Freqs = HashMap Text Entry

--------------------------------------------------------------------------------
-- | Insert an entry into the map.  If the entry is already in the
-- table keep the one with the lowest rank.
insert :: (Text, Entry) -> Freqs -> Freqs
insert (word, ent) = HashMap.insertWith go word ent
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
type Lists = HashMap Text (HashMap Text Int)

--------------------------------------------------------------------------------
-- | Given a @Freqs@ table, extract all the component lists.
extractLists :: Freqs -> Lists
extractLists = HashMap.foldrWithKey go HashMap.empty
  where
    go :: Text -> Entry -> Lists -> Lists
    go word Entry{..} = ins (name list) word rank

    ins :: Text -> Text -> Int -> Lists -> Lists
    ins lst word rnk = HashMap.insertWith HashMap.union lst (HashMap.singleton word rnk)

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
run :: Global -> IO ()
run Global{..} = do
  putStr (header $ fromMaybe "Text.Password.Strength.Generated.Frequency" mname)
  putStr "import Text.Password.Strength.Internal.Adjacency\n\n"

  ls <- extractLists <$> foldM (flip processList) HashMap.empty (map mkList files)

  forM_ (HashMap.toList ls) $ \(lst, tbl) ->
    putStrLn (encode (Text.unpack lst) "HashMap Text Int" tbl)
