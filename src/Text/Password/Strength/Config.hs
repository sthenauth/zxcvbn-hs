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
module Text.Password.Strength.Config (
  -- * Configuring word dictionaries, keyboards, etc.
  Config,
  Dictionary,
  addCustomFrequencyList,

  -- * Lenses for the 'Config' Type
  --
  -- | * 'passwordLists': Leaked password frequency lists.
  --
  --   * 'wordFrequencyLists': Various word frequency lists.
  --
  --   * 'customFrequencyLists': Custom word frequency lists.  Usually
  --      includes information about the person whose password we are
  --      guessing and the application for which we are running.  An
  --      easier way to add words to this list is to use the
  --      'addCustomFrequencyList' function.
  --
  --   * 'keyboardGraphs': Keyboard adjacency graphs.
  --
  --   * 'obviousSequenceStart': Predicate function that should return
  --     'True' for characters that are "obvious first choices" to
  --     start a sequence.  For example, in English, @a@ and @A@ would
  --     be considered 'True'.
  --
  passwordLists,
  wordFrequencyLists,
  customFrequencyLists,
  keyboardGraphs,
  obviousSequenceStart,
  HasConfig
  ) where

import Text.Password.Strength.Internal.Config
