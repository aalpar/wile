#!/bin/bash
# Download Larceny R7RS benchmark sources and inputs from GitHub.
# Re-run this script to refresh from upstream.
set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
BASEURL="https://raw.githubusercontent.com/larcenists/larceny/master/test/Benchmarking/R7RS"

SOURCES="common ack browse bv2string compiler conform cpstak ctak deriv destruc
  diviter divrec dynamic earley equal fib fibc fibfp fft gcbench graphs lattice
  matrix maze mazefun mbrot mbrotZ mperm nboyer nqueens ntakl nucleic paraffins
  parsing peval pi pnpoly primes puzzle quicksort ray sboyer scheme simplex
  slatex sum sumfp tak takl triangl"

INPUTS="ack browse bv2string compiler conform cpstak ctak deriv destruc diviter
  divrec dynamic earley equal fib fibc fibfp fft gcbench graphs lattice matrix
  maze mazefun mbrot mbrotZ mperm nboyer nqueens ntakl nucleic paraffins parsing
  peval pi pnpoly primes puzzle quicksort ray sboyer scheme simplex slatex sum
  sum1 sumfp tak takl triangl"

# Extra data files referenced by some benchmarks.
EXTRA_INPUTS="dynamic.data"

mkdir -p "$DIR/src" "$DIR/inputs"

echo "Downloading benchmark sources..."
for f in $SOURCES; do
    curl -sL "$BASEURL/src/$f.scm" -o "$DIR/src/$f.scm"
    echo "  src/$f.scm"
done

echo "Downloading benchmark inputs..."
for f in $INPUTS; do
    curl -sL "$BASEURL/inputs/$f.input" -o "$DIR/inputs/$f.input"
    echo "  inputs/$f.input"
done

echo "Downloading extra data files..."
for f in $EXTRA_INPUTS; do
    curl -sL "$BASEURL/inputs/$f" -o "$DIR/inputs/$f"
    echo "  inputs/$f"
done

echo "Done. $(ls "$DIR/src/"*.scm | wc -l | tr -d ' ') sources, $(ls "$DIR/inputs/"* | wc -l | tr -d ' ') inputs."
