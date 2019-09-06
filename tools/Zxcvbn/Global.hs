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
module Zxcvbn.Global
  ( Global(..)
  , global
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Options.Applicative

--------------------------------------------------------------------------------
data Global = Global
  { debug   :: Bool
  , mname   :: Maybe String
  , files   :: [FilePath]
  } deriving Show

--------------------------------------------------------------------------------
global :: Parser Global
global = Global
  <$> switch
        (mconcat [ long "debug"
                 , help "Enable debugging output"
                 ])

  <*> optional (strOption
        (mconcat [ short 'm'
                 , long "module"
                 , metavar "NAME"
                 , help "Use NAME as the generated module name"
                 ]))

  <*> some (argument str (metavar "FILE..."))
