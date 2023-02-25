#!/bin/sh

cabal haddock all > haddock.raw.txt
grep ") in " haddock.raw.txt | sort -hr > haddock.txt
COVERAGE=$(awk '{good += $3; total += $5} END {printf "%.0f", good/total*100}' haddock.txt)
echo "\
  { \"schemaVersion\": 1, \
    \"label\": \"api docs\", \
    \"message\": \"$COVERAGE%\", \
    \"color\": \"success\" \
  }" > haddock_badge.json
# rm haddock.raw.txt
cat haddock.txt
# rm haddock.txt
