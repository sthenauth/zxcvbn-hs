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
module Main (main) where

--------------------------------------------------------------------------------
-- Imports:
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime, utctDay)
import System.IO
import Text.Password.Strength

--------------------------------------------------------------------------------
-- | Read a password from stdin and score it.
main :: IO ()
main = do
  -- Turn off buffering for the password prompt below:
  hSetBuffering stdout NoBuffering

  -- Get the date and read a password:
  refDay   <- utctDay <$> getCurrentTime
  password <- putStr "Password: " >> Text.getLine

  -- Get the score for the password we just read:
  let s = score en_US refDay password

  -- Output some info about the score:
  putStrLn ("   Score: " ++ show (getScore s))
  putStrLn ("Strength: " ++ show (strength s))
