# Bibliography

Academic papers, specifications, and references used in the Wile Scheme implementation.

## Macro Hygiene

### Binding as Sets of Scopes (Flatt 2016)

The foundation for Wile's hygienic macro system.

- **Paper**: Matthew Flatt, "Binding as Sets of Scopes", POPL 2016
- **URL**: https://www.cs.utah.edu/plt/scope-sets/
- **DOI**: https://doi.org/10.1145/2837614.2837620

This paper introduces the "sets of scopes" model for macro hygiene, which Wile uses for `syntax-rules` macro expansion. Each identifier carries a set of scopes, and variable resolution checks that the binding's scopes are a subset of the use site's scopes.

## Language Specifications

### R7RS-small (Revised⁷ Report on the Algorithmic Language Scheme)

The primary language specification that Wile implements.

- **PDF**: https://small.r7rs.org/attachment/r7rs.pdf
- **HTML (Corrected)**: https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html
- **Homepage**: https://small.r7rs.org/
- **R7RS-large Wiki**: https://codeberg.org/scheme/r7rs/wiki

### R5RS (Revised⁵ Report on the Algorithmic Language Scheme)

Earlier Scheme standard, referenced for `scheme-report-environment` and `null-environment`.

- **URL**: https://www.schemers.org/Documents/Standards/R5RS/

## SRFIs (Scheme Requests for Implementation)

### SRFI-1: List Library

Canonical definitions for list processing procedures including `fold`.

- **URL**: https://srfi.schemers.org/srfi-1/srfi-1.html

### SRFI-9: Defining Record Types

Record type definitions, integrated into R7RS as `define-record-type`.

- **URL**: https://srfi.schemers.org/srfi-9/srfi-9.html

### SRFI-18: Multithreading Support

Threading primitives implemented in Wile: threads, mutexes, condition variables, and time objects.

- **URL**: https://srfi.schemers.org/srfi-18/srfi-18.html

### SRFI-141: Integer Division

Specification for integer division operations (`quotient`, `remainder`, `modulo`, `floor/`, `truncate/`).

- **URL**: https://srfi.schemers.org/srfi-141/srfi-141.html
- **Wiki**: https://small.r7rs.org/wiki/DivisionRiastradh/

### SRFI-170: POSIX API (Planned)

Comprehensive POSIX API for file system operations.

- **URL**: https://srfi.schemers.org/srfi-170/srfi-170.html

### SRFI-198: Foreign Object Error Handling (Planned)

Error handling for foreign function interfaces.

- **URL**: https://srfi.schemers.org/srfi-198/srfi-198.html

## Unicode Standards

### Unicode Case Folding

Referenced for `char-foldcase` and `string-foldcase` implementations.

- **CaseFolding.txt**: https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt
- **SpecialCasing.txt**: https://www.unicode.org/Public/UCD/latest/ucd/SpecialCasing.txt

### UAX #29: Unicode Text Segmentation

Unicode standard annex for text boundary analysis.

- **URL**: https://unicode.org/reports/tr29/

## Numeric Standards

### IEEE 754: Floating-Point Arithmetic

Standard for floating-point representation used by `Float` type.

- **Standard**: IEEE 754-2019 (ISO/IEC/IEEE 60559:2020)

## Tutorials and Learning Resources

### An Introduction to Scheme and its Implementation

Comprehensive Scheme tutorial covering implementation concepts.

- **URL**: https://www.cs.utexas.edu/ftp/garbage/cs345/schintro-v14/schintro_toc.html

## Related Systems

### Racket

Racket documentation referenced for `@`-expression reader syntax (planned feature).

- **Scribble Reader**: https://docs.racket-lang.org/scribble/reader.html
- **At-expressions**: https://docs.racket-lang.org/at-exp/index.html

### Go x/text Package

Used for Unicode case mapping operations.

- **Documentation**: https://pkg.go.dev/golang.org/x/text/cases

## Citation Format

When citing R7RS sections in code comments, use the format `R7RS §X.Y.Z`:

```go
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
```

Key R7RS sections:

| Section | Topic |
|---------|-------|
| §4.1-4.3 | Expressions, syntax |
| §5.1-5.5 | Program structure, definitions |
| §6.1 | Equivalence predicates |
| §6.2 | Numbers (tower, exactness, operations) |
| §6.3 | Booleans, pairs, lists, symbols, characters, strings, vectors |
| §6.4 | Bytevectors |
| §6.5 | Control features |
| §6.6 | Exceptions |
| §6.7-6.13 | Environments, I/O, system interface |
