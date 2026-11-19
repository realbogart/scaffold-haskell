#!/usr/bin/env bash
set -euo pipefail
cd -- "$(dirname -- "$0")"

exec ghcid --command="cabal repl exe:{{project_name}}" --restart=lib --test=main
