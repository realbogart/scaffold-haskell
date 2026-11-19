#!/usr/bin/env bash
set -euo pipefail

usage() {
    echo "Usage: scaffold-haskell PROJECT_DIR"
}

if [[ $# -eq 1 && ($1 == --help || $1 == -h) ]]; then
    usage
    exit 0
fi
[[ $# -eq 1 && -n $1 ]] || { usage >&2; exit 64; }

export LC_ALL=C
out_dir=$1
[[ $out_dir == /* ]] || out_dir="$PWD/$out_dir"
while [[ $out_dir == */ && $out_dir != / ]]; do
    out_dir=${out_dir%/}
done
project_name=${out_dir##*/}
parent=${out_dir%/*}

# Each Cabal package-name component must contain at least one letter.
component_pattern='[0-9]*[a-zA-Z][a-zA-Z0-9]*'
package_pattern="^${component_pattern}(-${component_pattern})*$"
if [[ ! $project_name =~ $package_pattern ]]; then
    echo "Invalid package name '$project_name'. Use letters, digits, and hyphens, with a letter in each component." >&2
    exit 64
fi

IFS=- read -ra components <<< "$project_name"
module_name=""
for component in "${components[@]}"; do
    module_name+=${component^}
done
case $module_name in
    [0-9]*) module_name="Project$module_name" ;;
    Main|Prelude) module_name+=Lib ;;
esac

if [[ -e $out_dir || -L $out_dir ]]; then
    echo "Destination already exists: $out_dir" >&2
    exit 1
fi

template_dir=${TEMPLATE_DIR:-"$(dirname -- "${BASH_SOURCE[0]}")/../templates"}
template_dir=$(realpath -e -- "$template_dir")
mkdir -p -- "${parent:-/}"
stage=$(mktemp -d -- "$parent/.scaffold-haskell.XXXXXXXX")
trap 'rm -rf -- "$stage"' EXIT
mkdir -- "$stage/project"
printf '{"project_name":"%s","project_name_capitalized":"%s"}\n' \
    "$project_name" "$module_name" > "$stage/data.json"

find "$template_dir" -type f -print0 | while IFS= read -r -d '' tpl; do
    rel=${tpl#"$template_dir"/}
    case "$rel" in
        template.cabal) rel="$project_name.cabal" ;;
        lib/template.hs) rel="lib/$module_name.hs" ;;
        'test/{{project_name_capitalized}}Spec.hs') rel="test/${module_name}Spec.hs" ;;
    esac
    dest="$stage/project/$rel"
    mkdir -p -- "$(dirname -- "$dest")"
    mustache "$stage/data.json" "$tpl" > "$dest"
    if [[ -x $tpl ]]; then
        chmod +x -- "$dest"
    fi
done

# Publish the complete tree in one rename, including when two renders race.
mv -Tn -- "$stage/project" "$out_dir"
if [[ -d $stage/project ]]; then
    echo "Destination already exists: $out_dir" >&2
    exit 1
fi

if command -v git >/dev/null; then
    git -C "$out_dir" init -q
    git -C "$out_dir" add .
    git -C "$out_dir" commit -qm "Initial commit" || true
fi

if command -v direnv >/dev/null; then
    (cd -- "$out_dir" && direnv allow) || true
fi

echo "Project rendered to $out_dir"
