#!/usr/bin/env bash
set -euo pipefail
cd -- "$(dirname -- "$0")/.."

cabal build all --enable-tests --enable-benchmarks
exec ./scripts/test.sh
