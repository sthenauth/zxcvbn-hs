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
-- Library Imports:
import Options.Applicative

--------------------------------------------------------------------------------
import qualified Zxcvbn.Adjacency as Adjacency
import qualified Zxcvbn.Freq as Freq
import qualified Zxcvbn.Global as Global

--------------------------------------------------------------------------------
data Tool
  = Adjacency
  | Frequency

--------------------------------------------------------------------------------
data Options = Options
  { tool   :: Tool
  , global :: Global.Global
  }

--------------------------------------------------------------------------------
options :: Parser Options
options = Options
  <$> commands
  <*> Global.global

  where
    commands :: Parser Tool
    commands = hsubparser $ mconcat
      [ command "adjacency"
          (info (pure Adjacency)
            (progDesc "Generate keyboard adjacency tables"))

      , command "frequency"
          (info (pure Frequency)
            (progDesc "Generate word frequency tables"))
      ]

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- execParser
            (info (options <**> helper)
              (mconcat [ fullDesc
                       , progDesc "Generate tables for zxcvbn"
                       , header "zxcvbn - Source code generators"
                       ]))

  case tool args of
    Adjacency -> Adjacency.run (global args)
    Frequency -> Freq.run (global args)
