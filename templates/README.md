# {{project_name}}

Run the executable with the pinned Nix toolchain.

```bash
nix run .
# Hello, world!
```

The executable in `src/Main.hs` delegates to `lib/{{project_name_capitalized}}.hs`. The Hspec suite in `test/` and the Criterion benchmarks in `bench/` exercise that library.

## Development

Enter the development shell with `nix develop`. Inside the shell, these commands work from the project root.

| Command | Purpose |
| --- | --- |
| `cabal run exe:{{project_name}}` | Build and run the executable |
| `./ghcid.sh` | Reload and run the executable after edits |
| `./scripts/format.sh` | Format the Haskell sources with Ormolu |
| `./scripts/format.sh --check` | Check formatting without editing files |
| `./scripts/verify.sh` | Compile every component and run the tests |
| `./scripts/test.sh` | Run Hspec and QuickCheck with live output |
| `./scripts/bench.sh` | Measure the library with Criterion |

To run a single command in the shell, use `nix develop -c ./scripts/verify.sh`.
