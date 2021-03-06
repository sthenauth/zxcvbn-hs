{-# LANGUAGE OverloadedStrings #-}

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
import Criterion.Main
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time.Calendar as Time
import qualified Text.Password.Strength as Zxcvbn

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
  [ go "password"
  , go "password1999"
  , go "passwordpasswordp@ssw0rd"
  , go "passwordasdftasdft"
  , go "YohMei9eeb6Ahrohrahk"
  , go "P!@'XrC4b%-#Ldcd9aqJ+`;'VfjnWe"
  , go "#]HtCNk#`#@)n\49Kq!m/i9@phtg)|Re'wNuy@}F^s|9ip$/v-*fFg:p<X9."
  ]

  where
    go :: Text -> Benchmark
    go p = bench (Text.unpack p) $
             whnf (Zxcvbn.getScore . Zxcvbn.score Zxcvbn.en_US refDay) p

    refDay :: Time.Day
    refDay = Time.fromGregorian 2019 1 1
