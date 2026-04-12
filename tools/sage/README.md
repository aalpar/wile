# Sage Algebra Validation

Cross-validates Wile's `(wile algebra)` libraries against SageMath.

## Requirements

- SageMath >= 10.8 (`sage -version`)
- Wile built (`make build`)

## Usage

    # Run both phases live (no file output)
    sage tools/sage/verify_algebra.sage

    # Structure validation only
    sage tools/sage/verify_algebra.sage --phase structures

    # Rewriting soundness only
    sage tools/sage/verify_algebra.sage --phase rewriting

    # Generate static .scm test files for CI
    sage tools/sage/verify_algebra.sage --snapshot

    # Custom RNG seed for exploration
    sage tools/sage/verify_algebra.sage --seed 123

## Design

See `plans/2026-04-12-sage-algebra-validation-design.md`.
