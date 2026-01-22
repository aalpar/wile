# CLAUDE.md

Package `system` provides system-level primitives.

## Purpose

- Command-line argument access
- Environment variable access
- Process exit
- Time measurement (jiffy-based)
- Feature detection

## Key Files

| File | Purpose |
|------|---------|
| `register.go` | Extension registration |
| `prim_system.go` | System primitive implementations |

## Primitives (Runtime only)

| Primitive | Args | Purpose |
|-----------|------|---------|
| `command-line` | 0 | Get command-line arguments as list |
| `exit` | 0-1 | Exit process (runs cleanup) |
| `emergency-exit` | 0-1 | Exit immediately (no cleanup) |
| `get-environment-variable` | 1 | Get env var value or #f |
| `get-environment-variables` | 0 | Get all env vars as alist |
| `current-second` | 0 | Get Unix timestamp as inexact |
| `current-jiffy` | 0 | Get monotonic jiffy count |
| `jiffies-per-second` | 0 | Get jiffy resolution |
| `features` | 0 | Get implementation features list |

## Usage

```go
import "wile/extensions/system"

// Use with registry
reg := registry.NewRegistry()
system.AddToRegistry(reg)
```

## Features List

The `features` primitive returns a list including:
- `r7rs` - R7RS compliance
- `wile` - Wile implementation
- Platform-specific features (darwin, linux, etc.)
- Architecture features (amd64, arm64, etc.)

## Gotchas

- **exit vs emergency-exit**: exit runs dynamic-wind after thunks; emergency-exit does not
- **Jiffy precision**: Resolution is nanoseconds (10^9 jiffies per second)
- **current-second**: Returns inexact real (floating point)
