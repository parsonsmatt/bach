# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test

```bash
stack build              # build library + executable
stack test               # run all tests
stack test --match "/buildBatches/"   # run tests matching a pattern
stack exec bach -- fugue --help       # run the CLI
fourmolu -i src/ app/ test/           # format all source files
```

## Architecture

Bach is a CLI tool that finds the largest conflict-free subset of GitHub PRs that can be merged together. It uses the `gh` CLI for GitHub interaction and `git merge-tree --write-tree` for in-memory merge testing (no worktrees or temp copies).

**Algorithm (three phases):**
1. **Pairwise conflict detection** — O(n²) `merge-tree` calls test every pair. For each left PR, merge it into base, create a temp commit (`git commit-tree`), then test each right PR against that temp commit.
2. **Graph coloring** — greedy assignment of PRs to the lowest batch number not used by any conflicting neighbor. Batch 1 is the candidate "ready" set.
3. **Sequential validation** — merge-tree each PR in batch 1 into an accumulated tree to catch higher-order conflicts the pairwise phase misses. Evicted PRs join the deferred set.

Output is `FugueResults { frReady, frDeferred, frBaseConflicts, frConflictPairs }`. Only batch 1 matters — after merging it, the remaining PRs need rebasing and you re-run bach.

**Module layout:**
- `Bach.Prelude` — re-exports RIO
- `Bach.Types` — all data types, JSON instances
- `Bach.Git` — git CLI wrappers (`merge-tree`, `commit-tree`, `fetch`, repo detection)
- `Bach.Forge` / `Bach.Forge.GitHub` — `MonadForge` class + `gh` CLI implementation (PR fetch, mark draft/ready)
- `Bach.Conflicts` — pairwise conflict detection (`partitionBase`, `findConflicts`)
- `Bach.Batching` — greedy graph coloring (`buildBatches`)
- `Bach.Fugue` — orchestrator: runs all three phases, returns `FugueResults`
- `Bach.Output` — human/JSON/gh-actions output formatting
- `Bach.App` — CLI parsing (optparse-applicative), app env setup, apply logic

**Key design decisions:**
- Git functions are plain `MonadIO m => FilePath -> ... -> m a` (no MonadGit class)
- Forge interaction uses `MonadForge` backed by a `ForgeHandle` record-of-functions, with the instance defined in `Bach.Forge` (not orphaned)
- `Map`'s `Semigroup` is left-biased `union`, not `unionWith (<>)` — use `Map.fromListWith` when accumulating into map values

## Code Style

- Uses `NoImplicitPrelude` with RIO — import `Bach.Prelude` (which re-exports `RIO`)
- Format with fourmolu (config in `fourmolu.yaml`): 4-space indent, 80-col limit, leading commas/arrows
- Use `BlockArguments` — omit `$` before lambdas and `do` blocks where fourmolu preserves it
- Use `OverloadedRecordDot` for field access (`pr.prNumber`), but note that `(.field)` sections require `HasField` in scope (import the defining module)
- Avoid list comprehensions — use `map`, `concatMap`, `mapMaybe` etc.
