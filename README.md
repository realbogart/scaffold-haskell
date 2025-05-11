# scaffold-haskell

Minimal Haskell project scaffolder powered by Nix flakes.

## Requirements

* **Nix 2.19+** with the experimental features **`flakes`** and **`nix-command`** enabled.

## Quick start

```bash
# create a new project called "coolapp" in your home directory
nix run --refresh github:realbogart/scaffold-haskell -- ~/coolapp

# navigate to the new project
cd ~/coolapp

# build and run using Nix
nix run .
```
