# CLAUDE.md

Package `gointerop` provides Go-specific concurrency primitives.

## Purpose

- Go channels for message passing
- WaitGroups for goroutine synchronization
- RWMutex for read-write locking
- Once for one-time initialization
- Atomic values for lock-free access

## Key Files

| File | Purpose |
|------|---------|
| `register.go` | Extension registration |
| `prim_channels.go` | Channel primitives |
| `prim_waitgroup.go` | WaitGroup primitives |
| `prim_rwmutex.go` | RWMutex primitives |
| `prim_once.go` | Once primitives |
| `prim_atomic.go` | Atomic value primitives |

## Primitives (Runtime only)

### Channels

| Primitive | Args | Purpose |
|-----------|------|---------|
| `make-channel` | 0-1 | Create channel (optional buffer size) |
| `channel?` | 1 | Check if value is a channel |
| `channel-send!` | 2 | Send value to channel (blocks) |
| `channel-receive` | 1 | Receive from channel (blocks) |
| `channel-try-send!` | 2 | Non-blocking send |
| `channel-try-receive` | 1 | Non-blocking receive |
| `channel-close!` | 1 | Close channel |
| `channel-closed?` | 1 | Check if channel is closed |
| `channel-length` | 1 | Get buffered element count |
| `channel-capacity` | 1 | Get buffer capacity |

### WaitGroup

| Primitive | Args | Purpose |
|-----------|------|---------|
| `make-wait-group` | 0 | Create WaitGroup |
| `wait-group?` | 1 | Check if value is a WaitGroup |
| `wait-group-add!` | 2 | Add to counter |
| `wait-group-done!` | 1 | Decrement counter |
| `wait-group-wait!` | 1 | Block until counter is zero |

### RWMutex

| Primitive | Args | Purpose |
|-----------|------|---------|
| `make-rw-mutex` | 0-1 | Create RWMutex |
| `rw-mutex?` | 1 | Check if value is an RWMutex |
| `rw-mutex-read-lock!` | 1 | Acquire read lock |
| `rw-mutex-read-unlock!` | 1 | Release read lock |
| `rw-mutex-write-lock!` | 1 | Acquire write lock |
| `rw-mutex-write-unlock!` | 1 | Release write lock |
| `rw-mutex-try-read-lock!` | 1 | Non-blocking read lock |
| `rw-mutex-try-write-lock!` | 1 | Non-blocking write lock |

### Once

| Primitive | Args | Purpose |
|-----------|------|---------|
| `make-once` | 0 | Create Once |
| `once?` | 1 | Check if value is a Once |
| `once-do!` | 2 | Execute thunk exactly once |
| `once-done?` | 1 | Check if already executed |

### Atomic

| Primitive | Args | Purpose |
|-----------|------|---------|
| `make-atomic` | 1 | Create atomic with initial value |
| `atomic?` | 1 | Check if value is atomic |
| `atomic-load` | 1 | Load value atomically |
| `atomic-store!` | 2 | Store value atomically |
| `atomic-swap!` | 2 | Swap and return old value |
| `atomic-compare-and-swap!` | 3 | CAS operation |

## Usage

```go
import "wile/extensions/gointerop"

// Use with registry
reg := registry.NewRegistry()
gointerop.AddToRegistry(reg)
```

## Gotchas

- **Go semantics**: Follows Go concurrency patterns
- **Blocking**: Channel operations block goroutines, not OS threads
- **Panic recovery**: Channel operations on closed channels may panic
