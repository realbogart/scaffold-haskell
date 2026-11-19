#!/usr/bin/env bash
set -euo pipefail

repo=$(cd -- "$(dirname -- "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd -- "$work"
nix run "$repo" -- --no-git --no-direnv sample-app
cd sample-app
printf 'Hello, world!\n' > ../expected
nix run "path:$PWD" > ../actual
diff -u ../expected ../actual
nix develop "path:$PWD" -c bash -ec '
    ./scripts/format.sh --check
    ./scripts/format.sh
    ./scripts/verify.sh
    ./scripts/bench.sh --benchmark-options="-n 1"
'
