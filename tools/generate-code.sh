#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix
# shellcheck shell=bash

if [ ! -d tools ]; then
  >&2 echo "ERROR: please run from the top-level directory"
  exit 1
fi

data=$(realpath data)
src=$(realpath src/Text/Password/Strength/Generated)

echo "==> Build"
nix-hs build

echo "==> Frequency.hs"
nix-hs \
  run zxcvbn-freq -- \
  "$data"/passwords/xato.txt:30000                       \
  "$data"/dictionaries/en-US/us_tv_and_film.txt:30000    \
  "$data"/dictionaries/en-US/english_wikipedia.txt:30000 \
  "$data"/dictionaries/en-US/surnames.txt:10000          \
  "$data"/dictionaries/en-US/male_names.txt              \
  "$data"/dictionaries/en-US/female_names.txt            \
  > "$src"/Frequency.hs

# Local Variables:
#   mode: sh
#   sh-shell: bash
# End:
