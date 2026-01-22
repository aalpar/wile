# CLAUDE.md

Package `files` provides file I/O primitives.

## Purpose

- File opening (text and binary modes)
- File existence and deletion
- Higher-order file operations (call-with-input-file, etc.)

## Key Files

| File | Purpose |
|------|---------|
| `register.go` | Extension registration |
| `prim_files.go` | Primitive implementations |

## Primitives (Runtime only)

| Primitive | Args | Purpose |
|-----------|------|---------|
| `open-input-file` | 1 | Open file for text input |
| `open-output-file` | 1 | Open file for text output |
| `open-binary-input-file` | 1 | Open file for binary input |
| `open-binary-output-file` | 1 | Open file for binary output |
| `file-exists?` | 1 | Check if file exists |
| `delete-file` | 1 | Delete a file |
| `call-with-input-file` | 2 | Call procedure with input port, close on return |
| `call-with-output-file` | 2 | Call procedure with output port, close on return |
| `with-input-from-file` | 2 | Set current-input-port during thunk |
| `with-output-to-file` | 2 | Set current-output-port during thunk |

## Usage

```go
import "wile/extensions/files"

// Use with registry
reg := registry.NewRegistry()
files.AddToRegistry(reg)
```

## Gotchas

- **Depends on io extension**: Uses port state from `extensions/io`
- **Port cleanup**: call-with-* and with-*-from/to-file ensure ports are closed
