# bach

*Bringing harmony to your pull requests.*

Bach finds the largest conflict-free set of PRs that can be merged together. When you have many PRs contending for the same codebase, bach composes them into a well-ordered **fugue** — voices that harmonize rather than clash.

## The Problem

You have 15 open PRs. Some conflict with each other, most don't, and you don't know which until you try. Merging them one at a time and hoping for the best is tedious. Merging them all and debugging the resulting cacophony is worse.

## The Solution

Bach tests every pair of PRs for merge conflicts using `git merge-tree` (in-memory, no worktree changes), builds a conflict graph, and uses graph coloring to find the largest set that can all be merged together without conflicts.

```
$ bach fugue 101 102 103 104 105

=== CONFLICT PAIRS ===
  #102 conflicts with #104
    files: src/Api.hs

=== READY TO MERGE (4) ===
  #101 Add user preferences endpoint
  #102 Refactor API middleware
  #103 Fix pagination bug
  #105 Update dependencies

=== DEFERRED (1) ===
  #104 New auth flow
```

Merge the ready set, rebase the deferred PRs, and run bach again. Repeat until the concert is over.

## Installation

Requires [`stack`](https://docs.haskellstack.org/) and the [`gh`](https://cli.github.com/) CLI.

```bash
git clone https://github.com/parsonsmatt/bach
cd bach
stack install
```

## Usage

### Analyze PRs

```bash
# Analyze by PR number
bach fugue 101 102 103 104 105

# Analyze by branch name
bach fugue feature/auth feature/api fix/pagination

# Mix and match
bach fugue 101 feature/api 103

# JSON output
bach fugue --json 101 102 103

# Pipeable gh commands
bach fugue --gh-actions 101 102 103
```

### Apply Results

```bash
# Analyze and apply (mark ready PRs as ready, draft the rest)
bach fugue --apply 101 102 103 104 105

# Or apply a saved plan later
bach apply-plan /tmp/bach-plan.json
```

### Options

```
bach fugue [--apply] [--base BRANCH] [--no-fetch] [--json | --gh-actions]
           [--plan-file FILE] PR...
```

| Flag | Description |
|------|-------------|
| `--apply` | Mark ready PRs as ready and deferred PRs as draft on GitHub |
| `--base BRANCH` | Base branch (default: detected from `origin/HEAD`) |
| `--no-fetch` | Skip `git fetch`, use local refs only |
| `--json` | Output results as JSON |
| `--gh-actions` | Output pipeable `gh` CLI commands |
| `--plan-file FILE` | Where to save the plan (default: `/tmp/bach-plan.json`) |

## How It Works

Bach's algorithm has three movements:

**I. Exposition** — Pairwise conflict detection. Every pair of PRs is tested with `git merge-tree --write-tree`, which checks mergeability in-memory without touching the working directory. For each left PR, bach merges it into the base branch, creates a temporary commit, and tests every other PR against that combined state. This is O(n^2) but each test is fast.

**II. Development** — Graph coloring. The conflict pairs form an undirected graph. Bach greedily assigns each PR to the lowest-numbered batch not used by any conflicting neighbor. Batch 1 is the largest conflict-free set.

**III. Recapitulation** — Sequential validation. The pairwise test can miss higher-order conflicts (where A+B and A+C merge cleanly, but A+B+C doesn't). Bach validates batch 1 by sequentially merging each PR into an accumulated tree. Any PR that fails is evicted to the deferred set.

The result is a ready set that is guaranteed to merge cleanly together.

## Why "Bach"?

Because managing PRs is like conducting a fugue — multiple independent voices that need to work in counterpoint. And because **B**atch + **A**nalyze + **Ch**eck = bach. Also, Johann Sebastian Bach composed the *Well-Tempered Clavier*, and what are merge conflicts if not poorly tempered keys?
