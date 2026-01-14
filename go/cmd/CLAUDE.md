# CLAUDE.md

Package `cmd` is the main entry point for the Wile Scheme interpreter.

## Purpose

Provides a command-line interface with:
- Interactive REPL with readline support
- File execution mode
- Integrated debugger with breakpoints and stepping
- Library path management via flags and environment variables

## Key Files

| File | Purpose |
|------|---------|
| `main.go` | Entry point, REPL loop, compilation pipeline |
| `debug_commands.go` | Debug command dispatcher and handlers |
| `errors.go` | BSD-style exit codes |

## Key Functions

- `compile(env, expr)` - Compiles expression through expansion and compilation phases
- `run(ctx, template, env)` / `runWithDebugger()` - Execute compiled templates
- `runREPL()` - Main REPL with readline, debug commands, multi-line input
- `runFile()` - Batch file execution
- `HandleDebugCommand()` - Parses comma-prefixed debug commands

## Debug Commands

Commands start with `,` and only work when input buffer is empty:
- `,b file:line[:col]` - Set breakpoint
- `,delete N` - Delete breakpoint
- `,list` - List breakpoints
- `,step`, `,next`, `,finish`, `,continue` - Execution control
- `,backtrace`, `,where` - Stack inspection

## Gotchas

- **Library path priority**: CLI paths (`-L`) are added last so they're searched first
- **Top-level compilation**: Uses `inTail=false` because top-level expressions aren't in tail position
- **Multi-line input**: Accumulates incomplete expressions; uses heuristic pattern matching on error messages
- **Readline fallback**: Silently falls back to simple REPL if readline initialization fails
- **Void suppression**: Results are only printed if not void

## Testing

Uses quicktest framework. Tests cover debug command parsing, breakpoint lifecycle, and location parsing.
