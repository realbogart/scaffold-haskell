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

Use `--no-git` or `--no-direnv` to skip either setup step. For example, to generate files inside another repository:

```bash
nix run github:realbogart/scaffold-haskell -- --no-git --no-direnv tools/cool-app
```

When the new project is part of an existing Git repository, stage its files before running its flake so Nix can see them.

## Working in a generated project

Run `nix develop -c ./scripts/verify.sh` to compile the library, executable, test suite, and benchmarks and run the tests. The generated README also covers formatting, live reload, and benchmarking.

## Working on the generator

Run `nix run . -- ./cool-app` to try local changes.

| Command | What it exercises |
| --- | --- |
| `nix flake check` | Rendering, module names, file permissions, destination protection, and Git initialization |
| `./scripts/check-project.sh` | Generate a temporary project, run the executable, check formatting, compile every Cabal component, run the tests, and smoke-test the benchmarks in the pinned development shell |
