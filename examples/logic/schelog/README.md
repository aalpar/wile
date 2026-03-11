# Schelog: Prolog in Scheme

Schelog is an embedding of Prolog-style logic programming in Scheme, created by
Dorai Sitaram. Wile runs the **unmodified** upstream schelog.scm — no patches, no
compatibility shims. This works because Schelog exercises exactly the features
Wile implements: first-class continuations (`call/cc`) for backtracking,
hygienic macros (`syntax-rules`) for the query DSL, and mutable state for the
trail. Having all three work correctly together, on unmodified third-party code,
is a concrete demonstration of Wile's R7RS language completeness.

## Quick Start

Start a REPL with schelog loaded:

```bash
./dist/wile -i -f examples/logic/schelog/schelog.scm
```

Try some queries:

```scheme
> (%which (x) (%member x '(a b c)))
((x a))
> (%more)
((x b))
> (%more)
((x c))
> (%more)
#f
```

Or load the demo for family relationships, append, and more:

```bash
./dist/wile -i -f examples/logic/schelog/schelog.scm -f examples/logic/schelog/demo.scm
```

## Running Examples

Load schelog first, then any example file. All of these complete in seconds:

```bash
# Basic predicates: append, reverse, factorial, length
./dist/wile -i -f examples/logic/schelog/schelog.scm -f examples/logic/schelog/toys.scm

# Map coloring (4-color theorem)
./dist/wile -i -f examples/logic/schelog/schelog.scm -f examples/logic/schelog/mapcol.scm

# Logic puzzle from Sterling & Shapiro
./dist/wile -i -f examples/logic/schelog/schelog.scm \
              -f examples/logic/schelog/puzzle.scm \
              -f examples/logic/schelog/games.scm

# Royal family relationships
./dist/wile -i -f examples/logic/schelog/schelog.scm -f examples/logic/schelog/england.scm

# Biblical genealogy with set predicates
./dist/wile -i -f examples/logic/schelog/schelog.scm -f examples/logic/schelog/bible.scm

# Simple facts database
./dist/wile -i -f examples/logic/schelog/schelog.scm -f examples/logic/schelog/holland.scm
```

### Multiple Files

The `-f` flag can be repeated to load multiple files in order:

```bash
./dist/wile -i -f lib1.scm -f lib2.scm -f main.scm
```

All files except the last are loaded silently. In interactive mode (`-i`), the
REPL starts after all files are loaded.

## Running Tests

The validation suite covers all fast examples (toys, holland, england, bible,
mapcol, games) and completes in seconds:

```bash
./examples/logic/schelog/run-all-tests.sh
```

Or via the single-process Scheme test runner:

```bash
./dist/wile -f examples/logic/schelog/run-all-tests.scm
```

## Key Concepts

- **Logic variables**: Created with `%let`, represent unknowns to be unified
- **Relations**: Defined with `%rel`, similar to Prolog predicates
- **Queries**: `%which` finds solutions, `%more` backtracks for alternatives
- **Unification**: `%=` unifies terms, `%is` evaluates arithmetic
- **Control**: `%and`, `%or`, `%not`, `!` (cut)
- **Sets**: `%bag-of`, `%set-of` collect all solutions
- **Occurs check**: `*schelog-use-occurs-check?*` enables for complex unification

## Files

### Core Library
- `schelog.scm` - Complete schelog library (unmodified from upstream)

### Examples (from upstream)
- `toys.scm` - Basic predicates: append, reverse, factorial, length
- `holland.scm` - Simple facts database
- `england.scm` - Royal family relationships
- `england2.scm` - Alternative Scheme-style syntax
- `bible.scm` - Biblical genealogy with set predicates
- `mapcol.scm` - Map coloring (4-color theorem)
- `games.scm` - Logic puzzle from Sterling & Shapiro
- `puzzle.scm` - Generic puzzle solver
- `houses.scm` - Zebra puzzle / Einstein's riddle (stress test)

### Wile-specific
- `demo.scm` - Interactive demonstration
- `benchmark.scm` - Fast benchmark (toys, mapcol, games)
- `stress-test.scm` - Zebra puzzle stress test
- `run-all-tests.scm` - Scheme-based validation suite
- `run-all-tests.sh` - Shell-based validation suite
- `README.md` - This file

## Stress Tests

The zebra puzzle (`houses.scm`) is a brute-force constraint satisfaction problem
that exercises heavy backtracking with occurs-check enabled. It takes significant
time in an interpreted Scheme — this is expected. Dedicated Prolog
implementations use constraint propagation and indexing to prune the search
space; Schelog's pure backtracking approach does not.

To run:

```bash
./dist/wile -f examples/logic/schelog/stress-test.scm
```

Or interactively:

```bash
./dist/wile -i -f examples/logic/schelog/schelog.scm \
              -f examples/logic/schelog/puzzle.scm \
              -f examples/logic/schelog/houses.scm
```

```scheme
> (set! *schelog-use-occurs-check?* #t)  ; Required for Zebra puzzle
> (solve-puzzle %houses)
((solution= ((japan owns the zebra) (norway drinks water))))
```

## Resources

- [Schelog Documentation](https://ds26gte.github.io/schelog/)
- [GitHub Repository](https://github.com/ds26gte/schelog)
- [Wile Scheme](https://github.com/aalpar/wile) - Pure Go Scheme with hygienic macros
