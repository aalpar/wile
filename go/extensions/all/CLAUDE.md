# CLAUDE.md

Package `all` provides additional primitives and aggregates all standard extensions.

## Purpose

- Aggregates all standard extensions (io, files, math, eval, exceptions, threads, gointerop, system)
- Provides record types (SRFI-9 style)
- Provides promises (delay/force)
- Provides additional string operations
- Provides additional character operations

## Key Files

| File | Purpose |
|------|---------|
| `register.go` | Extension registration and aggregation |
| `prim_records.go` | Record type primitives |
| `prim_promises.go` | Promise primitives |
| `prim_strings.go` | Additional string primitives |
| `prim_chars.go` | Additional character primitives |

## Included Extensions

This extension includes all of:
- `io` - Port I/O
- `files` - File I/O
- `system` - System interface
- `math` - Transcendental functions
- `exceptions` - Exception handling
- `eval` - Evaluation and environments
- `threads` - SRFI-18 threading
- `gointerop` - Go concurrency primitives

## Additional Primitives (Runtime only)

### Records (SRFI-9 style)

| Primitive | Args | Purpose |
|-----------|------|---------|
| `make-record-type` | 2 | Create record type |
| `record-type?` | 1 | Check if value is a record type |
| `record?` | 1 | Check if value is a record |
| `record-type` | 1 | Get record's type |
| `record-constructor` | 2 | Create constructor procedure |
| `record-predicate` | 1 | Create predicate procedure |
| `record-accessor` | 2 | Create field accessor |
| `record-modifier` | 2 | Create field modifier |

### Promises

| Primitive | Args | Purpose |
|-----------|------|---------|
| `promise?` | 1 | Check if value is a promise |
| `make-promise` | 1 | Create eager promise |
| `force` | 1 | Force promise evaluation |
| `%make-lazy-promise` | 1 | Create lazy promise (internal) |

### Additional String Operations

| Primitive | Args | Purpose |
|-----------|------|---------|
| `string-copy!` | 2-5 | Copy string to another |
| `string-fill!` | 2-4 | Fill string with character |
| `string-map` | 2+ | Map procedure over strings |
| `string-for-each` | 2+ | Apply procedure to strings |
| `string-ci=?` | 2+ | Case-insensitive equal |
| `string-ci<?` | 2+ | Case-insensitive less-than |
| `string-ci>?` | 2+ | Case-insensitive greater-than |
| `string-ci<=?` | 2+ | Case-insensitive less-or-equal |
| `string-ci>=?` | 2+ | Case-insensitive greater-or-equal |
| `string-upcase` | 1 | Convert to uppercase |
| `string-downcase` | 1 | Convert to lowercase |
| `string-foldcase` | 1 | Convert to foldcase |

### Additional Character Operations

| Primitive | Args | Purpose |
|-----------|------|---------|
| `char-ci=?` | 2+ | Case-insensitive equal |
| `char-ci<?` | 2+ | Case-insensitive less-than |
| `char-ci>?` | 2+ | Case-insensitive greater-than |
| `char-ci<=?` | 2+ | Case-insensitive less-or-equal |
| `char-ci>=?` | 2+ | Case-insensitive greater-or-equal |
| `char-alphabetic?` | 1 | Check if alphabetic |
| `char-numeric?` | 1 | Check if numeric |
| `char-whitespace?` | 1 | Check if whitespace |
| `char-upper-case?` | 1 | Check if uppercase |
| `char-lower-case?` | 1 | Check if lowercase |
| `char-upcase` | 1 | Convert to uppercase |
| `char-downcase` | 1 | Convert to lowercase |
| `char-foldcase` | 1 | Convert to foldcase |
| `digit-value` | 1 | Get numeric digit value |

## Usage

```go
import "wile/extensions/all"

// Use with registry - includes all extensions
reg := registry.NewRegistry()
all.AddToRegistry(reg)
```

## Gotchas

- **Includes everything**: Adding this extension includes all standard extensions
- **Lazy promises**: `%make-lazy-promise` is internal; use `delay` macro instead
- **Case folding**: Uses Go's unicode.ToLower for foldcase (simple case folding)
