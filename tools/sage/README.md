# Sage Algebra Validation

Cross-validates Wile's `(wile algebra)` libraries against SageMath.

## Requirements

- SageMath >= 10.8 (`sage -version`)
- Wile built (`make build`)

## Usage

Preferred (via make; selects the host binary and guards on Sage):

    make sage-verify      # run both phases live against the built binary
    make sage-snapshot    # regenerate static .scm snapshots in test/wile/sage-generated/

Direct invocation (advanced):

    sage tools/sage/verify_algebra.sage                    # both phases, live
    sage tools/sage/verify_algebra.sage --phase structures # structures only
    sage tools/sage/verify_algebra.sage --phase rewriting  # rewriting only
    sage tools/sage/verify_algebra.sage --snapshot         # write .scm files
    sage tools/sage/verify_algebra.sage --seed 123         # custom RNG seed

Set `WILE=<path>` to point at a specific wile binary (the make targets do this
for you). Snapshots are static and require no Sage at CI time.

## Design

See `memory/2026-04-12-sage-algebra-validation-design.md` and the coverage
extension `plans/2026-06-07-sage-oracle-coverage-design.md`.
