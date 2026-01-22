# CLAUDE.md

Package `io` provides I/O primitives for reading and writing.

## Purpose

- Port operations (read, write, display, newline)
- Port predicates and management (port?, close-port, etc.)
- String and bytevector ports
- Port parameter state management (current-input-port, current-output-port, current-error-port)
- Tokenizer and parser caching per port

## Key Files

| File | Purpose |
|------|---------|
| `register.go` | Extension registration and Builder |
| `state.go` | Port parameter state, tokenizer/parser caches |
| `prim_read_write.go` | read, write, display, newline implementations |
| `prim_ports.go` | Port predicates, string/bytevector ports |

## State Management

This package manages the global I/O port state. Key state includes:

```go
// Port parameters (current-input-port, current-output-port, current-error-port)
CurrentInputPortParam  *machine.Parameter
CurrentOutputPortParam *machine.Parameter
CurrentErrorPortParam  *machine.Parameter

// Caches using weak references
Tokenizers map[values.Value]weak.Pointer[tokenizer.Tokenizer]
Parsers    map[values.Value]weak.Pointer[parser.Parser]
```

## Key Functions

| Function | Purpose |
|----------|---------|
| `InitState()` | Initialize port parameters and caches |
| `ResetState()` | Reset state (for testing) |
| `GetCurrentInputPort()` | Get current input port from parameter |
| `GetCurrentOutputPort()` | Get current output port from parameter |
| `GetCurrentErrorPort()` | Get current error port from parameter |
| `SetCurrentInputPort(port)` | Set current input port (testing) |
| `SetCurrentOutputPort(port)` | Set current output port (testing) |

## Primitives

### Read/Write (Runtime only)

| Primitive | Args | Purpose |
|-----------|------|---------|
| `read` | 0-1 | Read S-expression from port |
| `read-token` | 0-1 | Read single token from port |
| `read-syntax` | 0-1 | Read syntax object from port |
| `write` | 1-2 | Write S-expression to port |
| `write-char` | 1-2 | Write character to port |
| `display` | 1-2 | Display value (human-readable) |
| `newline` | 0-1 | Write newline to port |
| `write-simple` | 1-2 | Write without shared structure |
| `write-shared` | 1-2 | Write with shared structure markers |

### Port Predicates (Runtime only)

| Primitive | Args | Purpose |
|-----------|------|---------|
| `port?` | 1 | Check if value is a port |
| `input-port?` | 1 | Check if value is an input port |
| `output-port?` | 1 | Check if value is an output port |
| `input-port-open?` | 1 | Check if input port is open |
| `output-port-open?` | 1 | Check if output port is open |
| `close-port` | 1 | Close a port |
| `close-input-port` | 1 | Close an input port |
| `close-output-port` | 1 | Close an output port |
| `eof-object` | 0 | Return the EOF object |
| `eof-object?` | 1 | Check if value is EOF |

### String Ports (Runtime only)

| Primitive | Args | Purpose |
|-----------|------|---------|
| `open-input-string` | 1 | Create input port from string |
| `open-output-string` | 0 | Create output string port |
| `get-output-string` | 1 | Get accumulated string from port |

### Bytevector Ports (Runtime only)

| Primitive | Args | Purpose |
|-----------|------|---------|
| `open-input-bytevector` | 1 | Create input port from bytevector |
| `open-output-bytevector` | 0 | Create output bytevector port |
| `get-output-bytevector` | 1 | Get accumulated bytevector from port |

## Usage

```go
import ioext "wile/extensions/io"

// Use with registry
reg := registry.NewRegistry()
ioext.AddToRegistry(reg)

// Or use Extension directly with Engine
engine, _ := wile.NewEngine()  // io is included by default

// Access port state
port := ioext.GetCurrentOutputPort()
ioext.SetCurrentInputPort(myPort)
```

## Gotchas

- **Weak references**: Tokenizers/parsers are cached with weak pointers to allow GC
- **InitState idempotent**: Safe to call multiple times; subsequent calls are no-ops
- **Port parameters**: Created as `machine.Parameter` objects, not regular values
