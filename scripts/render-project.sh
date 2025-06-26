#!/usr/bin/env bash
set -euo pipefail

[[ $# -eq 1 ]] || { echo "Usage: $0 /absolute/output/dir" >&2; exit 64; }

OUT_DIR="${1%/}"
[[ $OUT_DIR = /* ]] || { echo "❌OUT_DIR must be an absolute path"; exit 64; }

PROJECT_NAME=$(basename "$OUT_DIR")
PROJECT_NAME_CAPITALIZED=${PROJECT_NAME^}

TEMPLATE_DIR="${TEMPLATE_DIR:-templates}"

command -v mustache >/dev/null || { echo "❌mustache CLI not found"; exit 1; }

mkdir -p "$OUT_DIR"

DATA_FILE=$(mktemp)
printf '{"project_name":"%s","project_name_capitalized":"%s"}' \
       "$PROJECT_NAME" "$PROJECT_NAME_CAPITALIZED" >"$DATA_FILE"

find "$TEMPLATE_DIR" -type f | while read -r tpl; do
    rel=${tpl#"$TEMPLATE_DIR"/}
    case "$rel" in
        template.cabal) rel="${PROJECT_NAME}.cabal" ;;
        lib/template.hs) rel="lib/${PROJECT_NAME_CAPITALIZED}.hs" ;;
    esac
    dest="$OUT_DIR/$rel"
    mkdir -p "$(dirname "$dest")"
    mustache "$DATA_FILE" "$tpl" >"$dest"
    chmod u+rw "$dest"
    if [[ ${rel##*/} == "ghcid.sh" ]] || [[ $rel == scripts/*.sh ]]; then
        chmod u+wx "$dest"
    fi
    :
done

rm -f "$DATA_FILE"

if command -v git >/dev/null; then
    cd "$OUT_DIR"
    git init -q
    git add .
    git commit -qm "Initial commit" || true
fi

command -v direnv >/dev/null && (cd "$OUT_DIR" && direnv allow) || true

echo "✅ Project rendered to $OUT_DIR"
