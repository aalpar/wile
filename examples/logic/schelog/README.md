# Schelog: Prolog in Scheme

Schelog is an embedding of Prolog-style logic programming in Scheme, created by
Dorai Sitaram. This directory contains the **unmodified** schelog.scm library
and all example files from the upstream repository, demonstrating Wile's full
compatibility with Schelog.

## Compatibility

Wile runs Schelog without any modifications to the original source code. All
upstream examples pass, including the famous Zebra puzzle. Run the validation
suite to verify:

```bash
./examples/logic/schelog/run-all-tests.sh
```

Expected output:
```
=== Schelog Validation Suite for Wile ===
...
=== Test Summary ===
Passed: 16
Failed: 0

All tests passed! Wile is fully compatible with Schelog.
```

## Usage

### Interactive Exploration

Start a REPL with schelog loaded:

```bash
./dist/scheme -i -f examples/logic/schelog/schelog.scm
```

Then try:

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

### Running the Demo

```bash
./dist/scheme -i -f examples/logic/schelog/schelog.scm -f examples/logic/schelog/demo.scm
```

### Running Examples

Load schelog first, then any example file:

```bash
# Map coloring
./dist/scheme -i -f examples/logic/schelog/schelog.scm -f examples/logic/schelog/mapcol.scm

# Zebra puzzle (requires puzzle.scm helper)
./dist/scheme -i -f examples/logic/schelog/schelog.scm \
              -f examples/logic/schelog/puzzle.scm \
              -f examples/logic/schelog/houses.scm
```

Then query interactively:

```scheme
> (set! *schelog-use-occurs-check?* #t)  ; Required for Zebra puzzle
> (solve-puzzle %houses)
((solution= ((japan owns the zebra) (norway drinks water))))
```

### Multiple Files

The `-f` flag can be repeated to load multiple files in order:

```bash
./dist/scheme -i -f lib1.scm -f lib2.scm -f main.scm
```

All files except the last are loaded silently. In interactive mode (`-i`), the
REPL starts after all files are loaded.

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
- `houses.scm` - Zebra puzzle (Einstein's riddle)

### Wile-specific
- `demo.scm` - Interactive demonstration
- `run-all-tests.sh` - Validation suite
- `README.md` - This file

## Resources

- [Schelog Documentation](https://ds26gte.github.io/schelog/)
- [GitHub Repository](https://github.com/ds26gte/schelog)
- [Wile Scheme](https://github.com/aalpar/wile) - Pure Go Scheme with hygienic macros
