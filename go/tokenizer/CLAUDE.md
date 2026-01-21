# CLAUDE.md

Package `tokenizer` implements R7RS Scheme lexical analysis.

## Purpose

Converts Unicode rune stream into tokens:
- Complete R7RS token support (70+ token types)
- Complex number formats (rectangular, polar, imaginary)
- Arbitrary precision numbers (#z, #m prefixes)
- Extended symbols, escape sequences, nested comments
- Precise source position tracking

## Key Types

**Tokenizer** - Main lexer:
- `rdr` - Input rune reader
- `cur` - Current rune
- `state` - Current token type (TokenizerState)
- `value` - Processed value (escapes resolved)
- `text` - Raw source text

**Token** - Interface with `Type()`, `Start()`, `End()`, `String()`, `Value()`

**TokenizerState** - 70+ constants for token classification

## Key Functions

- `NewTokenizer(reader, caseInsensitive)` - Create tokenizer
- `Next()` - Get next token (returns io.EOF at end)
- `Tokenize(string, ci)` - Convenience for entire string

## Token Categories

| Category | Examples |
|----------|----------|
| Delimiters | `(`, `)`, `'`, `` ` ``, `,` |
| Numbers | SignedInteger, SignedDecimalFraction, SignedComplex |
| Special | SignedInf, SignedNan, SignedImaginary |
| Radix | MarkerBase2/8/10/16, BigInteger, BigFloat |
| Literals | Symbol, String, Character |
| Comments | LineComment, BlockComment, DatumComment |
| Vectors | OpenVector, OpenVectorUnsignedByteMarker |

## Gotchas

- **Radix state reset**: Must reset to 0 after parsing #b/#o/#d/#x numbers
- **Case sensitivity**: Booleans always case-insensitive; symbols depend on `ci` flag
- **text vs value**: `text` is raw source, `value` has escapes resolved
- **Dot ambiguity**: Could be decimal point, cons operator, or symbol prefix
- **Nested block comments**: Depth counter tracks `#|...|#` nesting
- **No backtracking**: Parser commits once input consumed
- **Position tracking**: Updates happen after character consumed
- **String escape sequences**: R7RS uses `\xHEX;` for Unicode escapes (e.g., `\x41;` for 'A'); `\U` escape is not valid—embed Unicode characters directly or use `\xHEX;` format

## Testing

Uses quicktest with extensive table-driven tests for all token types, numeric formats, and error cases.

### Test File Organization

Tests are organized with the main test file plus coverage-focused supplementary files:

| Test File | Purpose |
|-----------|---------|
| `tokenizer_test.go` | Core tokenizer functionality |
| `big_number_test.go` | BigInteger/BigFloat token parsing |
| `imaginary_coverage_test.go` | Imaginary number handling |
| `edge_cases_test.go` | Edge case coverage |
| `error_path_coverage_test.go` | Error scenario coverage |
| `*_coverage_test.go` | Additional coverage files |

This organization ensures comprehensive coverage while keeping test files focused and maintainable.

## References

See `BIBLIOGRAPHY.md` at project root for R7RS §7.1.1 (Lexical structure).
