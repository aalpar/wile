# Testing Scheme Compatibility

The Scheme test suite is designed to work with any R7RS-compatible Scheme implementation. This allows you to:

1. **Verify R7RS conformance** — Ensure Wile behaves like other R7RS Schemes
2. **Test compatibility** — Check if code runs on multiple implementations
3. **Compare implementations** — See behavioral differences across Schemes
4. **Regression testing** — Compare Wile versions against each other

## Quick Start

**Test with Wile (default):**
```bash
make test-scheme
```

**Test with another Scheme implementation:**
```bash
make test-scheme SCHEME=chez-scheme
make test-scheme SCHEME=chibi-scheme
make test-scheme SCHEME=gauche
```

## Supported Implementations

The test suite uses `(chibi test)`, which is available in:

| Implementation | Package | Command |
|----------------|---------|---------|
| **Wile** | Built-in | `make test-scheme` |
| **Chibi-Scheme** | Built-in | `make test-scheme SCHEME=chibi-scheme` |
| **Chez Scheme** | Requires manual installation of chibi-test | `make test-scheme SCHEME=chez-scheme` |
| **Gauche** | May require manual installation | `make test-scheme SCHEME=gosh` |

**Note**: Most R7RS Schemes do not include `(chibi test)` by default. You may need to install it separately or bundle it with your tests.

## Comparing Wile Versions

**Test current version:**
```bash
make build
make test-scheme
```

**Test old version:**
```bash
# Save old binary
cp ./dist/darwin/arm64/scheme ./dist/darwin/arm64/scheme-v1.0.0

# Build new version
make build

# Compare
make test-scheme SCHEME=./dist/darwin/arm64/scheme-v1.0.0
make test-scheme  # Current version
```

## Example: Comparing Numeric Tower Behavior

Create a test file `test/scheme/numeric-comparison.scm`:

```scheme
(import (scheme base)
        (scheme inexact)
        (chibi test))

(test-begin "numeric-tower")

(test-group "exactness"
  (test #t (exact? 1))
  (test #f (exact? 1.0))
  (test #t (exact? (+ 1 2)))
  (test #f (exact? (+ 1.0 2))))

(test-group "division"
  (test 1/2 (/ 1 2))           ; Rational result
  (test 0.5 (exact->inexact (/ 1 2))))

(test-end)
```

Run against multiple implementations:

```bash
make test-scheme                        # Wile
make test-scheme SCHEME=chez-scheme     # Chez Scheme
make test-scheme SCHEME=chibi-scheme    # Chibi-Scheme
```

## Troubleshooting

### Test fails with "import: unknown library"

The target Scheme doesn't have `(chibi test)`. Options:

1. Install chibi-test for that Scheme (if available)
2. Bundle `lib/chibi/test.scm` with your distribution
3. Use a different test framework (SRFI-64)

### Test syntax not supported

Some Schemes have slightly different R7RS support. Check:

- Is the Scheme R7RS-small compliant?
- Does it support `(import ...)` syntax?
- Are library paths configured correctly?

### Different numeric behavior

This is expected! R7RS allows implementation-defined behavior for:

- Numeric tower support (some Schemes don't have exact rationals)
- Precision of inexact numbers
- Rounding modes

Document these differences in test comments or skip incompatible tests.

## Best Practices

1. **Write portable tests** — Use only R7RS-small features in core tests
2. **Document assumptions** — If a test requires specific behavior, note it
3. **Separate implementation-specific tests** — Keep Wile-specific tests separate
4. **Test early, test often** — Run compatibility checks before releases

## See Also

- `test/README.md` — Test suite documentation
- `CONTRIBUTING.md` — Testing guidelines
- [R7RS-small specification](https://small.r7rs.org/)
