# internal/ — Tokenizer, Parser, Syntax, Validate, Match, Bootstrap

## R7RS Conformance

This project aims to implement R7RS-small. Key resources:

| Source | URL |
|--------|-----|
| R7RS-small PDF | https://small.r7rs.org/attachment/r7rs.pdf |
| R7RS Corrected (HTML) | https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html |
| R7RS-large Wiki (in progress) | https://codeberg.org/scheme/r7rs/wiki |

**Testing policy**: Tests that conform to R7RS must not be removed or reverted. If a test fails but correctly reflects R7RS behavior, the implementation must be fixed — not the test.

## Compile-Time Code

Compile-time/macro code uses `*Pair` only (no `ArrayList` at those phases). The `Tuple` interface is for runtime read-only operations.
