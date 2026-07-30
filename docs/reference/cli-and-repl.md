# CLI and REPL Reference

This document describes the `wile` command-line interface, the interactive
REPL, and the in-REPL debugger.

## Command Line

### Synopsis

```
wile [options] [file...]
```

With no arguments, `wile` enters the interactive REPL. Positional arguments
are treated as Scheme files to load before evaluation begins. Files can also
be passed via `-f`/`--file`. A bare `-` names standard input, positionally or
as `--file -`.

### Options

| Short | Long             | Type       | Description |
|-------|------------------|------------|-------------|
| `-e`  | `--eval`         | string (repeatable) | Evaluate a Scheme expression |
| `-f`  | `--file`         | string (repeatable) | Load a Scheme file |
| `-i`  | `--interactive`  | bool       | Enter REPL after loading file(s) |
| `-L`  | `--library-path` | string     | Library search paths (colon-separated) |
| `-q`  | `--quiet`        | bool       | Suppress informational messages |
| `-V`  | `--version`      | bool       | Print version and exit |
|       | `--mcp`          | bool       | Start as MCP server on stdio |
|       | `--mcp-timeout`  | float      | Default eval timeout in seconds for MCP mode (default: 30) |
|       | `--cpuprofile`   | string     | Write CPU profile to file |
|       | `--memprofile`   | string     | Write memory profile to file |
|       | `--mutexprofile` | string     | Write mutex contention profile to file |
|       | `--blockprofile` | string     | Write goroutine blocking profile to file |
|       | `--cover`        | string     | Write Scheme-level coverage report to file (Go cover format) |
|       | `--cover-stdlib` | bool       | Include stdlib files in `--cover` output (default excludes `scheme/`, `wile/`, `srfi/`) |
|       | `--cover-summary`| string     | Write human-readable coverage summary to file |

`--` terminates flag parsing; everything after is a positional argument.

### Environment Variables

| Variable               | Effect |
|------------------------|--------|
| `SCHEME_LIBRARY_PATH`  | Additional library search paths, colon-separated. Appended to `-L`. |

### Examples

```bash
wile                                    # Start REPL
wile program.scm                        # Run file and exit
wile -f program.scm                     # Same, via flag
wile -f program.scm -i                  # Run file, then enter REPL
wile -e '(+ 1 2)'                       # Evaluate expression and print result
wile -L /path/to/libs program.scm       # Add library search path
wile -e '(define x 10)' -e '(* x x)'    # Multiple expressions, in order
```

## REPL

The REPL supports readline-style line editing and persists definitions across
expressions. All core primitives and any loaded extensions are available.

```
> (define (fib n)
    (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
> (fib 10)
55
> (map fib '(0 1 2 3 4 5 6 7 8 9 10))
(0 1 1 2 3 5 8 13 21 34 55)
```

Multi-line expressions are accepted; the REPL continues reading until the
expression is complete. Use `Ctrl-D` (EOF) or `(exit)` to quit.

### Meta Commands

Meta commands begin with `,` and operate on the REPL session itself rather than
evaluating Scheme. Most accept a short alias.

| Command              | Aliases  | Description |
|----------------------|----------|-------------|
| `,help [command]`    | `,h`, `,?` | Show all commands, or detailed help for one |
| `,doc [-x] <name>`   |          | Show documentation for a binding or library. `-x` includes examples |
| `,doc (<lib>)`       |          | Show library description, source, and exports |
| `,edit <file>`       |          | Open file in `$EDITOR` (REPL blocks until editor exits) |
| `,apropos <pattern>` | `,a`     | Search bindings by name, documentation, or category |
| `,topics`            |          | List documentation categories with entry counts |
| `,topic <category>`  |          | List bindings in a category |
| `,libraries`         | `,libs`  | List loaded Scheme libraries with descriptions |
| `,disassemble <name>`| `,dis`   | Show bytecode disassembly of a procedure |
| `,version`           |          | Show interpreter version and build identifier |

For ad-hoc disassembly of an expression rather than a named procedure, use the
`disassemble` Scheme primitive directly: `(disassemble (lambda (x) (* x x)))`.

### Debugger Commands

The debugger is built into the REPL. Setting a breakpoint, stepping, or
hitting an error suspends execution and returns control to the prompt, where
the following commands are available.

| Command                | Aliases | Description |
|------------------------|---------|-------------|
| `,break FILE:LINE[:COL]` | `,b`  | Set a breakpoint at a source location |
| `,delete <id>`         | `,d`    | Delete a breakpoint by ID |
| `,list`                | `,l`    | List all breakpoints |
| `,enable <id>`         |         | Enable a breakpoint |
| `,disable <id>`        |         | Disable a breakpoint |
| `,step`                | `,s`    | Step into the next expression |
| `,next`                | `,n`    | Step over (stay in current frame) |
| `,finish`              | `,f`    | Step out (return from current function) |
| `,continue`            | `,c`    | Resume execution until the next breakpoint |
| `,backtrace`           | `,bt`   | Show the current continuation stack |
| `,where`               |         | Show the current source location |

Breakpoints survive across continuation resumes — they are attached to source
locations, not to particular VM states. A breakpoint set inside a procedure
will fire on every call.

## See Also

- [`docs/reference/scheme.md`](scheme.md) — Complete Scheme language reference
- [`docs/embedding/api-design.md`](../embedding/api-design.md) — Embedding the interpreter in Go
- [`docs/extensions/libraries.md`](../extensions/libraries.md) — R7RS library import syntax
