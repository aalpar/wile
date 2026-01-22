# CLAUDE.md

Package `threads` provides SRFI-18 threading primitives.

## Purpose

- Thread creation and management
- Mutexes for synchronization
- Condition variables for coordination
- Time operations

## Key Files

| File | Purpose |
|------|---------|
| `register.go` | Extension registration |
| `prim_threads.go` | Thread primitives |
| `prim_mutexes.go` | Mutex primitives |
| `prim_condvars.go` | Condition variable primitives |
| `prim_time.go` | Time primitives |

## Primitives (Runtime only)

### Threads

| Primitive | Args | Purpose |
|-----------|------|---------|
| `current-thread` | 0 | Get current thread |
| `thread?` | 1 | Check if value is a thread |
| `make-thread` | 1-2 | Create thread from thunk |
| `thread-name` | 1 | Get thread name |
| `thread-specific` | 1 | Get thread-local value |
| `thread-specific-set!` | 2 | Set thread-local value |
| `thread-start!` | 1 | Start thread execution |
| `thread-yield!` | 0 | Yield to scheduler |
| `thread-sleep!` | 1 | Sleep for duration |
| `thread-terminate!` | 1 | Terminate thread |
| `thread-join!` | 1-2 | Wait for thread completion |

### Mutexes

| Primitive | Args | Purpose |
|-----------|------|---------|
| `mutex?` | 1 | Check if value is a mutex |
| `make-mutex` | 0-1 | Create mutex |
| `mutex-name` | 1 | Get mutex name |
| `mutex-specific` | 1 | Get mutex-local value |
| `mutex-specific-set!` | 2 | Set mutex-local value |
| `mutex-state` | 1 | Get mutex state |
| `mutex-lock!` | 1-2 | Lock mutex |
| `mutex-unlock!` | 1-2 | Unlock mutex |

### Condition Variables

| Primitive | Args | Purpose |
|-----------|------|---------|
| `condition-variable?` | 1 | Check if value is a condvar |
| `make-condition-variable` | 0-1 | Create condition variable |
| `condition-variable-name` | 1 | Get condvar name |
| `condition-variable-specific` | 1 | Get condvar-local value |
| `condition-variable-specific-set!` | 2 | Set condvar-local value |
| `condition-variable-signal!` | 1 | Signal one waiter |
| `condition-variable-broadcast!` | 1 | Signal all waiters |

### Time

| Primitive | Args | Purpose |
|-----------|------|---------|
| `current-time` | 0 | Get current time |
| `time?` | 1 | Check if value is a time |
| `time->seconds` | 1 | Convert time to seconds |
| `seconds->time` | 1 | Convert seconds to time |

## Usage

```go
import "wile/extensions/threads"

// Use with registry
reg := registry.NewRegistry()
threads.AddToRegistry(reg)
```

## Gotchas

- **SRFI-18 semantics**: Follows SRFI-18 specification
- **Go goroutines**: Threads implemented using Go goroutines
- **Blocking operations**: Some operations may block the Go runtime
