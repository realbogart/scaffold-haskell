#!/usr/bin/env bash

set -e

echo "Formatting Haskell files..."
find src/ lib/ -name "*.hs" -exec ormolu --mode inplace {} \;
echo "Formatting complete!"
