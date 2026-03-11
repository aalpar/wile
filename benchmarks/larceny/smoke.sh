#!/bin/bash
# Smoke test: run each benchmark with minimal iterations to check compatibility.
# Usage: ./smoke.sh [benchmark-name]
set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
SCHEME="$DIR/wile"
LIBDIR="$(cd "$DIR/../.." && pwd)/lib"
SRC="$DIR/src"
INPUTS="$DIR/inputs"
TMP="$DIR/tmp"

mkdir -p "$TMP"

run_bench() {
    local name="$1"
    local input="$2"

    # Concatenate source + common
    cat "$SRC/$name.scm" "$SRC/common.scm" > "$TMP/$name.scm"

    echo -n "  $name: "
    if echo "$input" | timeout 30 "$SCHEME" -q -L "$LIBDIR" --file "$TMP/$name.scm" 2>&1; then
        echo "  -> OK"
    else
        echo "  -> FAILED (exit $?)"
    fi
}

# Minimal-iteration inputs for smoke testing.
# Format matches what each benchmark's (main) reads via (read).

echo "=== Smoke testing Larceny R7RS benchmarks ==="

run_bench fib      "1 10 55"
run_bench tak      "1 18 12 6 7"
run_bench ack      "1 3 9 4093"
run_bench cpstak   "1 18 12 6 7"
run_bench ctak     "1 18 12 6 7"
run_bench deriv    "1 (+ (* 3 x x) (* a x x) (* b x) 5) (+ (+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x))) (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x))) (* (* b x) (+ (/ 0 b) (/ 1 x))) 0) (+ (* (* 3 x x) (+ 0 (+ (/ (- 1 0) (* x x))) (+ (/ (- 1 0) (* x x))))) (* (* a x x) (+ 0 (+ (/ (- 1 0) (* x x))) (+ (/ (- 1 0) (* x x))))) (* (* b x) (+ 0 (+ (/ (- 1 0) (* x x))))) 0)"
run_bench primes   "1 1000 (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997)"
run_bench nqueens  "1 8 92"
run_bench sum      "1 10000 50005000"
run_bench fib      "1 20 6765"
run_bench fibc     "1 10 55"
run_bench fibfp    "1 20.0 6765.0"
run_bench sumfp    "1 1000000.0 500000500000.0"
run_bench equal    "1 100 8 1000 2000 5000"
run_bench mazefun  "1 11 ()"
run_bench gcbench  "1 10 0"

echo ""
echo "=== Smoke test complete ==="
