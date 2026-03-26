# OS Primitives Design — SRFI-170 Subset

**Status:** Complete Phase 1 (PR #565)
**Date:** 2026-03-24
**Scope:** Directory operations + structured process control (SRFI-170 subset, option B)
**Delivery:** Two phases, two PRs
**Deferred:** SRFI-170 library wrapper (re-export with standards-compliant names)

---

## Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Scope | Minimal OS scripting + structured process control | Covers the useful SRFI-170 subset without signals/sockets/terminals |
| Extension structure | Directory ops in `files`, process ops in new `process` | Finer-grained sandboxing — embedders who want `directory-files` don't get `system` |
| Shell vs structured | Both, separate security actions | `ActionExecShell` vs `ActionExec` — authorizers can allow one without the other |
| Output model | Port-based process object | Matches existing port pattern, enables streaming |
| Stdin | Always connected | `process-stdin` always returns a writable port |
| Signals | Symbol argument vocabulary | `term`, `kill`, `int`, `hup` |
| Process value type | Opaque `*Process` in `values/` | Matches port convention for OS resources |
| Error handling | New `WrapForeignProcessError` | Parallel to `WrapForeignFileError`, carries command name |
| SRFI-170 library | Deferred | Can be added later without changing primitives |

---

## Phase 1: Directory Operations

**Extension:** `files` (existing)
**PR scope:** ~100 lines + tests

### Primitives

| Primitive | Params | Return | Security | Go impl |
|-----------|--------|--------|----------|---------|
| `create-directory` | 1: path string | void | `ResourceFile` / `ActionWrite` / target=path | `os.Mkdir(path, 0755)` |
| `delete-directory` | 1: path string | void | `ResourceFile` / `ActionDelete` / target=path | `os.Remove(path)` |
| `directory-files` | 1: path string | list of strings | `ResourceFile` / `ActionRead` / target=path | `os.ReadDir(path)`, filter `.`/`..` |
| `current-directory` | 0 | string | None | `os.Getwd()` |
| `set-current-directory!` | 1: path string | void | `ResourceProcess` / `ActionWrite` / target=`"cwd"` | `os.Chdir(path)` |

### Notes

- `directory-files` returns filenames only (not full paths), excludes `.` and `..`
- `create-directory` is single-level only (no recursive `mkdir -p`)
- `delete-directory` fails if directory is not empty (matches POSIX `rmdir`)
- Errors use existing `WrapForeignFileError`

### `set-current-directory!` — `os.Chdir` Semantics

`os.Chdir` is process-global. Multiple engines in the same Go process share a
single working directory. Concurrent calls from different goroutines race on the
same OS state. This is inherent to POSIX — there is no per-thread working
directory.

Document in:
- Code comment on the primitive implementation
- `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md`

---

## Phase 2: Process Extension

**Extension:** `process` (new)
**PR scope:** ~300-400 lines + tests
**Not in `SafeExtensions()`** — embedders opt in explicitly.

### New Security Actions (`security/access.go`)

```go
ActionExec      = "exec"       // structured process execution (process-spawn)
ActionExecShell = "exec-shell" // shell command execution (system)
```

Both use `ResourceProcess`. Target is the command name/string.

### New Value Type (`values/`)

```go
type Process struct {
    cmd    *exec.Cmd
    stdin  *CharacterOutputPort  // pipe to process stdin
    stdout *CharacterInputPort   // pipe from process stdout
    stderr *CharacterInputPort   // pipe from process stderr
}
```

- Implements `values.Value`
- `TypeName()` returns `"process"`
- Display: `#<process "ls" pid=1234>`

### Primitives

| Primitive | Params | Return | Security | Behavior |
|-----------|--------|--------|----------|----------|
| `system` | 1: command string | integer (exit code) | `ResourceProcess` / `ActionExecShell` / target=command | `/bin/sh -c <cmd>` |
| `process-spawn` | 2 (variadic): command, args... | `*Process` | `ResourceProcess` / `ActionExec` / target=command | `exec.Command`, start, return process |
| `process-stdout` | 1: process | input port | None | Accessor |
| `process-stderr` | 1: process | input port | None | Accessor |
| `process-stdin` | 1: process | output port | None | Accessor |
| `process-wait` | 1: process | integer (exit code) | None | Block until exit |
| `process-kill` | 2: process, signal symbol | void | None | Send OS signal |
| `process?` | 1: obj | boolean | None | Type predicate |

### `process-spawn` Argument Shape

```
ParamCount: 2, IsVariadic: true
Arg(0) = command string
Arg(1) = rest list of string args
```

```scheme
(process-spawn "ls" "-la" "/tmp")
```

### Signal Vocabulary

| Symbol | Signal | Notes |
|--------|--------|-------|
| `term` | `SIGTERM` | Graceful termination |
| `kill` | `SIGKILL` | Immediate termination |
| `int`  | `SIGINT`  | Interrupt |
| `hup`  | `SIGHUP`  | Hangup |

Unrecognized symbol raises error with `werr.ErrInvalidArgument`.

### New Error Type (`werr/`)

```go
type ForeignProcessError struct {
    Op      string // e.g., "process-spawn", "process-wait"
    Command string // the command that was run
    Err     error  // underlying OS error
}

func WrapForeignProcessError(err error, op string, command string) *ForeignProcessError
```

Parallel to `ForeignFileError`. Implements `error`, `Unwrap()`.

### Usage Examples

```scheme
;; Shell one-liner
(system "ls -la | grep foo")  ; => exit code integer

;; Structured process with port I/O
(let ((proc (process-spawn "grep" "error")))
  (display "some error text\n" (process-stdin proc))
  (close-output-port (process-stdin proc))
  (let ((line (read-line (process-stdout proc))))
    (process-wait proc)
    line))

;; Kill a long-running process
(let ((proc (process-spawn "sleep" "999")))
  (process-kill proc 'term)
  (process-wait proc))
```

---

## Files to Touch

### Phase 1
- `extensions/files/register.go` — add 5 primitive specs
- `extensions/files/prim_files.go` — add 5 implementations (or new `prim_directory.go`)
- `extensions/files/prim_files_test.go` — tests (or new `prim_directory_test.go`)
- `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` — document `os.Chdir` semantics

### Phase 2
- `security/access.go` — add `ActionExec`, `ActionExecShell`
- `security/security_test.go` — test new action constants
- `values/process.go` — new `*Process` type
- `values/process_test.go` — type tests
- `werr/werr.go` — add `ForeignProcessError`, `WrapForeignProcessError`
- `werr/werr_test.go` — tests
- `extensions/process/doc.go` — package doc
- `extensions/process/register.go` — extension registration
- `extensions/process/prim_process.go` — implementations
- `extensions/process/prim_process_test.go` — tests
