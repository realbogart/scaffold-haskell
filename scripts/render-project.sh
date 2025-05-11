#!/usr/bin/env bash
set -euo pipefail

##############################################################################
#  Usage: ./render-project.sh /absolute/path/to/PROJECT_NAME
##############################################################################

[[ $# -eq 1 ]] || { echo "Usage: $0 /absolute/output/dir" >&2; exit 64; }

OUT_DIR="${1%/}"
[[ $OUT_DIR = /* ]] || { echo "❌OUT_DIR must be an absolute path"; exit 64; }

PROJECT_NAME=$(basename "$OUT_DIR")
[[ -n $PROJECT_NAME ]] || { echo "❌Cannot derive project name"; exit 64; }

#— pick store path from wrapper if present, else local fallback —#
TEMPLATE_DIR="${TEMPLATE_DIR:-templates}"

command -v mustache >/dev/null || { echo "❌mustache CLI not found"; exit 1; }

mkdir -p "$OUT_DIR"

DATA_FILE=$(mktemp)
printf '{"project_name":"%s"}' "$PROJECT_NAME" >"$DATA_FILE"

find "$TEMPLATE_DIR" -type f | while read -r tpl; do
    rel=${tpl#"$TEMPLATE_DIR"/}
    dest="$OUT_DIR/${rel/template.cabal/${PROJECT_NAME}.cabal}"
    mkdir -p "$(dirname "$dest")"
    mode=$(stat -c %a "$tpl")
    mustache "$DATA_FILE" "$tpl" >"$dest"
    chmod "$mode" "$dest"
done

rm -f "$DATA_FILE"
cd "$OUT_DIR"
git init -q
git add .
git commit -qm "Initial commit"
direnv allow || true

echo "✅ Project rendered to $OUT_DIR"
