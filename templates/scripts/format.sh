#!/usr/bin/env bash
set -euo pipefail
cd -- "$(dirname -- "$0")/.."

mode=inplace
if [[ $# -eq 1 && $1 == --check ]]; then
    mode=check
elif [[ $# -ne 0 ]]; then
    echo "Usage: $0 [--check]" >&2
    exit 64
fi
find src lib test bench -type f -name '*.hs' -exec ormolu --mode "$mode" {} +
