# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell project built with Nix flakes for reproducible development environments and haskell.nix for building. The project follows strict code quality standards with comprehensive warning flags and formatting requirements.

## Development Workflow

**ALWAYS run these commands after making changes to Haskell source:**
```bash
./scripts/format.sh
./scripts/verify.sh
```

These scripts ensure code formatting and successful builds. Never skip this step before committing changes.

## Development Commands

**Enter development shell:**
```bash
nix develop
```

**Build and run:**
```bash
nix run .
```

**Build with cabal (in nix shell):**
```bash
cabal build
cabal run {{project_name}}
```

**Live reload development:**
```bash
./ghcid.sh
```
Uses ghcid to watch for changes and rebuild/test automatically.

**Build Docker container:**
```bash
nix build .#container
```

## Test-Driven Development (TDD)

**ALWAYS follow the TDD cycle when adding new functionality:**

### When to Apply TDD

**Use TDD for:**
- Business logic and algorithms
- Functions with complex behavior or edge cases  
- Data transformations and validation
- Error handling and boundary conditions
- Any functionality where bugs would be costly

**Consider skipping TDD for:**
- Simple getters/setters or obvious record accessors
- Trivial string formatting or basic I/O
- Straightforward plumbing code with no logic
- When prototyping or exploring design alternatives

**Default to TDD** - When in doubt, write the test first. It's better to have a test you didn't need than to miss a bug you should have caught.

### TDD Workflow: Red → Green → Refactor

1. **Red**: Write a failing test first
   ```bash
   ./scripts/test.sh  # Should fail
   ```

2. **Green**: Write minimal code to make the test pass
   ```bash
   ./scripts/test.sh  # Should pass
   ```

3. **Refactor**: Improve code while keeping tests green
   ```bash
   ./scripts/format.sh
   ./scripts/test.sh
   ./scripts/verify.sh
   ```

### TDD Commands

**Run tests continuously during development:**
```bash
./ghcid.sh  # Includes test runs on file changes
```

**Run tests manually:**
```bash
./scripts/test.sh
```

**Run tests in nix shell:**
```bash
cabal test
```

### Writing Effective Tests

**Use Hspec for behavior specification:**
```haskell
describe "MyModule.myFunction" $ do
  it "should handle empty input" $ do
    myFunction [] `shouldBe` []
  
  it "should process valid input" $ do
    myFunction [1,2,3] `shouldBe` [2,4,6]
```

**Use QuickCheck for property-based testing:**
```haskell
it "should preserve length" $ property $
  \xs -> length (myFunction xs) == length (xs :: [Int])
```

### TDD Best Practices

1. **Start with the simplest failing test** - Don't try to test everything at once
2. **Write descriptive test names** - Use "should" statements that describe behavior
3. **Test edge cases** - Empty lists, zero values, boundary conditions
4. **Use property-based tests** - Let QuickCheck find edge cases you might miss
5. **Keep tests fast** - Slow tests discourage frequent running
6. **Test behavior, not implementation** - Focus on what the function should do
7. **Refactor tests too** - Keep test code clean and maintainable

### Integration with Development Workflow

**Before starting new work:**
```bash
./scripts/test.sh  # Ensure all existing tests pass
```

**During TDD cycles:**
```bash
# 1. Write failing test, then:
./scripts/test.sh

# 2. Write minimal implementation, then:
./scripts/test.sh

# 3. Refactor and verify:
./scripts/format.sh
./scripts/test.sh
./scripts/verify.sh
```

**Before committing:**
```bash
./scripts/format.sh
./scripts/verify.sh  # Includes test run
```

## Code Quality Standards

**Formatting:**
- Always use ormolu for formatting (handled by `scripts/format.sh`)
- Never manually format - let the script handle it

**Warnings:**
- ALWAYS fix all compiler warnings - never ignore them
- Disable warnings in the cabal file's `common warnings` section, NEVER in source files
- The project uses `-Weverything` with specific exclusions for practical development

**Language Extensions:**
- Check the cabal file's `common extensions` section to see which extensions are enabled by default
- Current extensions include: OverloadedStrings, DuplicateRecordFields, NoFieldSelectors, StrictData, OverloadedRecordDot, AllowAmbiguousTypes, DataKinds, TypeFamilies, RecordWildCards, DeriveDataTypeable

## Architecture

**Project Structure:**
- `src/Main.hs` - Executable entry point that delegates to library
- `lib/` - Library modules with core functionality
- `scripts/` - Development automation scripts
- `flake.nix` - Nix flake defining the development environment and build

**Development Environment:**
- GHC 9.10.1 via haskell.nix
- Includes hlint, HLS, ormolu, and ghcid in development shell
- Comprehensive warning flags for strict code quality
- Uses GHC2021 language standard

**Dependencies:**
- Core dependencies: base, text, bytestring, async, unordered-containers
- Build tools: cabal, hlint, haskell-language-server available in nix shell

## Important Reminders

1. **Follow TDD: Write tests first, then implement**
2. **Always run format and verify scripts after changes**
3. **Fix all warnings - never disable them in source code**
4. **Use the cabal file to understand enabled language extensions**
5. **Enter nix develop shell for all development work**
6. **Use ghcid for continuous feedback during development**
7. **Run tests frequently - they should always pass**
8. **Test builds before committing any changes**
9. **NEVER make git commits - leave that to the user**