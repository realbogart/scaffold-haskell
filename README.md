# scaffold-haskell

Minimal Haskell project scaffolder powered by Nix flakes.

## Requirements

* **Nix 2.19+** with the experimental features **`flakes`** and **`nix-command`** enabled.

## Quick start

```bash
# create a new project called "coolapp" under ~/projects
nix run github:realbogart/scaffold-haskell -- ~/projects/coolapp

# navigate to the new project
cd ~/projects/coolapp

# build and run using Nix
nix run .
```
