# CLAUDE.md

Package `repl` provides an independent Read-Eval-Print Loop for Wile Scheme.

## Purpose

Provides a standalone REPL that can be:
- Used directly by `cmd/main.go`
- Embedded in custom applications
- Extended with custom debug commands

## Key Types

| Type | Purpose |
|------|---------|
| `REPL` | Main REPL type with readline support and debugging |
| `DebugContext` | Manages debugger state and handles debug commands |
| `Option` | Functional options for configuring REPL |

## Key Functions

- `New(env, opts...)` - Creates a new REPL with optional configuration
- `REPL.Run(ctx)` - Runs the REPL with readline support
- `REPL.RunSimple(ctx)` - Runs a basic REPL without readline
- `REPL.Debugger()` - Returns the debugger for external configuration

## Options

| Option | Purpose |
|--------|---------|
| `WithHistoryFile(path)` | Set history file path |
| `WithPrompt(prompt)` | Set primary prompt (default: "> ") |
| `WithContinuationPrompt(prompt)` | Set continuation prompt (default: "  ") |
| `WithOutput(w)` | Set output writer |
| `WithErrorOutput(w)` | Set error output writer |

## Debug Commands

Commands start with `,` and only work when input buffer is empty:

| Command | Aliases | Description |
|---------|---------|-------------|
| `,break FILE:LINE[:COL]` | `,b` | Set breakpoint |
| `,delete ID` | `,d` | Delete breakpoint |
| `,list` | `,l` | List breakpoints |
| `,enable ID` | - | Enable breakpoint |
| `,disable ID` | - | Disable breakpoint |
| `,step` | `,s` | Step into |
| `,next` | `,n` | Step over |
| `,finish` | `,f` | Step out |
| `,continue` | `,c` | Continue execution |
| `,backtrace` | `,bt` | Show stack trace |
| `,where` | - | Show current location |
| `,help` | `,h`, `,?` | Show help |

## Usage Example

```go
import (
    "context"
    "wile/repl"
    "wile/runtime"
)

func main() {
    env, _ := runtime.NewTopLevelEnvironmentFrameTiny(context.TODO())
    r := repl.New(env,
        repl.WithPrompt("scheme> "),
        repl.WithHistoryFile("/tmp/history"),
    )
    r.Run(context.Background())
}
```

## Gotchas

- **Readline fallback**: Silently falls back to simple REPL if readline fails
- **Multi-line input**: Accumulates incomplete expressions using heuristic error detection
- **Void suppression**: Results are only printed if not void
- **Debug context**: Debug commands only work when input buffer is empty
- **Context cancellation**: REPL checks context between iterations
