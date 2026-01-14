# CLAUDE.md

Package `parser` implements R7RS Scheme syntax parsing.

## Purpose

Converts token stream into syntax values with source location tracking:
- Parse all Scheme datums (literals, symbols, lists, vectors)
- Handle complex numeric types (rationals, complex, big numbers)
- Support quote forms and datum labels
- Preserve source location metadata

## Key Types

**Parser** - Main parser:
- `toks` - Underlying tokenizer (created lazily)
- `env` - Environment for symbol interning
- `cur` - Current token
- `file` - Source filename for error reporting

**ParserError** - Error with source location

## Key Functions

- `NewParserWithFile(env, skipComments, reader, file)` - Create parser
- `ReadSyntax(ctx)` - Read next syntax value
- `parseIntegerWithBase()`, `parseBigIntegerWithBase()` - Number parsing
- `parseRational()`, `parseComplex()`, `parsePolarComplex()` - Complex numbers
- `wrapSyntax*()` - Wrap values with source context

## Gotchas

- **Lazy tokenizer**: Created on first `ReadSyntax()` call
- **Case-insensitive prefixes**: Uses custom `TrimPrefixFolded()` for radix prefixes
- **Complex number edge cases**: Handles `+i`, `-i`, infnan in both parts
- **Polar conversion**: `r@θ` converted to rectangular form
- **No error recovery**: On error, tokenizer nullified
- **Comment skipping optional**: `skipComment` parameter controls behavior
- **Byte vector validation**: Elements validated as unsigned bytes at parse time

## Testing

Uses quicktest with comprehensive table-driven tests covering all token types, numeric formats, and edge cases.
