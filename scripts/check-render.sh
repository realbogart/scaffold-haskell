#!/usr/bin/env bash
set -euo pipefail

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd -- "$work"

# Record direnv calls without loading flakes or touching a user's allow list.
mkdir bin
printf '#!%s\n' "$BASH" > bin/direnv
cat >> bin/direnv <<'EOF'
printf '%s %s\n' "$PWD" "$*" >> "$DIRENV_CALLS"
EOF
chmod +x bin/direnv
export PATH="$work/bin:$PATH" DIRENV_CALLS="$work/direnv-calls"

reject() {
    if scaffold-haskell --no-git --no-direnv "$@"; then
        echo "Unexpected success for: $*" >&2
        exit 1
    fi
}

dest="with spaces/cool-app"
scaffold-haskell --no-git --no-direnv "$dest/"
test -f "$dest/.envrc"
test ! -e "$dest/.git"
for script in "$dest"/scripts/*.sh "$dest/ghcid.sh"; do test -x "$script"; done
grep -F 'module CoolApp (' "$dest/lib/CoolApp.hs"
grep -F 'exposed-modules:  CoolApp' "$dest/cool-app.cabal"
grep -F 'module CoolAppSpec (spec)' "$dest/test/CoolAppSpec.hs"
if grep -rF '{{' "$dest"; then exit 1; fi

echo 'keep me' > "$dest/README.md"
reject "$dest"
grep -Fx 'keep me' "$dest/README.md"
ln -s missing dangling
reject dangling
test -L dangling
for name in app-123 bad_name -leading trailing- a--b; do
    reject -- "$name"
    test ! -e "$name"
done
reject
reject ''
reject one two
test ! -e one
reject --unknown

# Exercise valid names with special Haskell module mappings.
for pair in '3d-view:Project3dView' 'main:MainLib' 'prelude:PreludeLib'; do
    name=${pair%:*}
    module=${pair#*:}
    scaffold-haskell --no-git --no-direnv "$PWD/$name"
    test -f "$name/lib/$module.hs"
    test -f "$name/test/${module}Spec.hs"
done

# Creating a project from a Git hook must leave the caller's index alone.
git init -q caller
export GIT_AUTHOR_NAME=Scaffolder GIT_AUTHOR_EMAIL=scaffolder@example.org
export GIT_COMMITTER_NAME=$GIT_AUTHOR_NAME GIT_COMMITTER_EMAIL=$GIT_AUTHOR_EMAIL
GIT_DIR="$PWD/caller/.git" GIT_WORK_TREE="$PWD/caller" \
    scaffold-haskell --no-direnv my-project
git -C my-project ls-tree --name-only HEAD | grep -Fx my-project.cabal
test -z "$(git -C caller ls-files)"
test ! -e "$DIRENV_CALLS"
scaffold-haskell --no-git with-direnv
grep -Fx "$PWD/with-direnv allow" "$DIRENV_CALLS"
