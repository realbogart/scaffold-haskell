#!/usr/bin/env bash

set -euo pipefail

##############################################################################
#  Usage: ./render-project.sh /absolute/path/to/PROJECT_NAME
##############################################################################

[[ $# -eq 1 ]] || { echo "Usage: $0 /absolute/output/dir" >&2; exit 64; }

OUT_DIR="${1%/}"
[[ $OUT_DIR = /* ]] || {
    echo "❌OUT_DIR must be an absolute path: '$OUT_DIR'" >&2
    exit 64
}

PROJECT_NAME=$(basename "$OUT_DIR")
[[ -n $PROJECT_NAME ]] || {
    echo "❌Failed to derive project name from '$OUT_DIR'" >&2
    exit 64
}

TEMPLATE_DIR="templates"

command -v mustache >/dev/null || {
    echo "❌mustache CLI not found" >&2
    exit 1
}

mkdir -p "$OUT_DIR"

DATA_FILE=$(mktemp)
printf '{"project_name":"%s"}' "$PROJECT_NAME" >"$DATA_FILE"

find "$TEMPLATE_DIR" -type f | while read -r tpl; do
    rel=${tpl#"$TEMPLATE_DIR"/}

    if [[ $rel == "template.cabal" ]]; then
        dest="$OUT_DIR/${PROJECT_NAME}.cabal"
    else
        dest="$OUT_DIR/$rel"
    fi

    mkdir -p "$(dirname "$dest")"

    # preserve permissions
    mode=$(stat -c %a "$tpl")

    mustache "$DATA_FILE" "$tpl" >"$dest"
    chmod "$mode" "$dest"
done

rm -f "$DATA_FILE"

cd "$OUT_DIR"

git init
git add .
git commit -m "Initial commit"

direnv allow

echo "✅ Project rendered to $OUT_DIR"
