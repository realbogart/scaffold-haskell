# scaffold-haskell

Generate a Haskell project with Cabal and a Nix development shell.

## Quick start

With Nix 2.19+ and the `flakes` and `nix-command` features enabled, run:

```bash
nix run github:realbogart/scaffold-haskell -- ~/cool-app
cd ~/cool-app
nix run .
```

Pass a new directory, using a relative or absolute path. The generator creates missing parents and refuses an existing destination. The directory name becomes the Cabal package name. Use letters and digits separated by optional hyphens, with at least one letter per component. For example, `cool-app` gets a library module named `CoolApp`.

When Git is available, the generator initializes a repository, stages the files, and attempts an initial commit. When direnv is available, it runs `direnv allow` in the new project.

## Working on the generator

Run `nix run . -- ./cool-app` to try local changes. Run `nix flake check` to test rendering, name validation, and protection of existing projects.
