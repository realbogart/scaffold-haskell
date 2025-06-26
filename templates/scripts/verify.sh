#!/usr/bin/env bash

set -e

echo "Building project..."
cabal build
echo "Build successful!"
