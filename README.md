# Password Strength Estimation

[![CI](https://github.com/sthenauth/zxcvbn-hs/workflows/CI/badge.svg)](https://github.com/sthenauth/zxcvbn-hs/actions)
[![GitHub tag (latest by date)](https://img.shields.io/github/v/tag/sthenauth/zxcvbn-hs?label=release)](https://github.com/pjones/zxcvbn-hs/releases)
[![Hackage](https://img.shields.io/hackage/v/zxcvbn-hs)](https://hackage.haskell.org/package/zxcvbn-hs)

## What?

This is a native Haskell implementation of the [zxcvbn][] password
strength estimation algorithm as it appears in the 2016 USENIX
Security [paper and presentation][paper] (with some small
modifications).

## Why?

The [zxcvbn][] algorithm is a major improvement over traditional
password strength estimators.  Instead of counting the occurrence of
special characters, mixed case characters, numeric digits, etc.,
zxcvbn analyzes a plain text password and estimates the number of
guesses that an attacker would need to make in order to crack it.

## How?

A plain text password is broken into a list of substrings called
tokens and each token is analyzed as follows:

  * Membership in a password or word frequency dictionary

  * Reversing the token and testing it against said dictionaries

  * Decoding l33t speak and testing it against said dictionaries

  * Determine if the token forms a pattern on a keyboard
    (e.g., "asdfgt", "poiuy", "aSw2@", etc.)

  * Compare the code points of the characters to see if they form a
    sequence (e.g., "13579", "abcde", "zyx", etc.)

  * Attempt to parse the token as a date with or without separators
    (e.g., "1013", "2011-01-01", "23/01/19", "012319", etc.)

  * Search for adjacent tokens that are identical (i.e. repeating
    patterns)

Each possible interpretation of a token is given an estimated number
of guesses and then the entire password is scored based on the weakest
path.

## Usage

A complete example can be found in the
[example/Main.hs](example/Main.hs) file.  That said, it's pretty easy
to use:

```haskell
import Text.Password.Strength (score, strength, en_US)
import Data.Time.Clock (getCurrentTime, utctDay)

main = do
  -- The date matcher needs to know the current year.
  refDay <- utctDay <$> getCurrentTime

  let password = "password1234567"
      guesses  = score en_US refDay password

  print guesses -- Number of estimated guesses (18)
  print (strength guesses) -- Sum type describing the password strength (Risky)
```

## Demo App

If you want to play with an interactive demo take a look at the
[zxcvbn-ws repository][zxcvbn-ws].

## Customization

You'll most likely want to add custom words to the frequency
dictionaries.  For example, the name of your application, your domain
name, and any personal information you have on the customer.  Doing so
will penalize the score of a password using such information.

The `Text.Password.Strength.Config` module defines the
`addCustomFrequencyList` function which can be used to easily add
words to the frequency dictionary.

### Localization

Unlike other implementations of the [zxcvbn][] algorithm, this version
fully supports localization.  It's easy to augment or completely
replace the frequency dictionaries and keyboard layouts.  Tools are
provided to compile simple text files into the data types required by
this library.

However, like the other implementations, the default configuration is
heavily biased towards United States English, hence its name: `en_US`.

Included in the default configuration are:

  * 30,000 most frequently used passwords according to [Mark Burnett][]

  * 30,000 most frequently used words in US movies and television shows

  * 30,000 most frequently used words in Wikipedia English articles

  * Top 10,000 surnames

  * Top 4,275 female names

  * Top 1,219 male names

  * QWERTY keyboard layout

  * Number pad keyboard layout

### Existing Localization Packages

  * [zxcvbn-dvorak][] Dvorak keyboard layout

## Performance

It takes approximately 1.5 ms to process a 30-character password.
Performance degrades as the length of the password increases (e.g., a
60-character password clocks in at 13.54 ms).

You probably want to limit the number of characters you send through
the `score` function using something like `Text.take 100` in order to
prevent a malicious user from slowing down your application.

Most of the time is currently spent in decoding and testing l33t
speak.  If you want to work on improving the performance I suggestion
you generate a profile using the benchmark tool.

[Mark Burnett]: https://xato.net/today-i-am-releasing-ten-million-passwords-b6278bbe7495?gi=d98e0d16566b
[paper]: https://www.usenix.org/conference/usenixsecurity16/technical-sessions/presentation/wheeler
[zxcvbn-dvorak]:https://code.devalot.com/sthenauth/zxcvbn-dvorak
[zxcvbn-ws]: https://code.devalot.com/sthenauth/zxcvbn-ws
[zxcvbn]: https://github.com/dropbox/zxcvbn
