#!/usr/bin/env bash
set -euo pipefail
cd -- "$(dirname -- "$0")/.."

exec cabal test --test-show-details=direct "$@"
