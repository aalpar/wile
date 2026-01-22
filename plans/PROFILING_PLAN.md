# Wile Profiling Tools Implementation Plan

## Overview

Add pprof profiling support to Wile via CLI flags. Profile data outputs to stdout/stderr for logging integration, or to files for analysis with `go tool pprof`.

## Profile Types Supported

| Type | Go Function | Description |
|------|-------------|-------------|
| CPU | `pprof.StartCPUProfile/StopCPUProfile` | CPU time sampling |
| Heap | `pprof.WriteHeapProfile` | Memory allocations (live) |
| Allocs | `pprof.Lookup("allocs").WriteTo` | Total allocations |
| Mutex | `pprof.Lookup("mutex").WriteTo` | Mutex contention |

## Implementation

### 1. CLI Flags (`go/cmd/main.go`)

Add to `Options` struct (around line 30):

```go
ProfileCPU    string `long:"profile-cpu" description:"CPU profile output: stdout, stderr, or file path"`
ProfileHeap   string `long:"profile-heap" description:"Heap profile output: stdout, stderr, or file path"`
ProfileAllocs string `long:"profile-allocs" description:"Allocations profile output: stdout, stderr, or file path"`
ProfileMutex  string `long:"profile-mutex" description:"Mutex profile output: stdout, stderr, or file path"`
```

### 2. Helper Functions (`go/cmd/main.go`)

Add output resolution helper:

```go
import (
    "runtime/pprof"
)

// nopCloser wraps an io.Writer to satisfy io.WriteCloser without closing.
type nopCloser struct{ io.Writer }
func (nopCloser) Close() error { return nil }

// resolveProfileWriter returns an io.WriteCloser for the given destination.
func resolveProfileWriter(dest string) (io.WriteCloser, error) {
    switch dest {
    case "stdout":
        return nopCloser{os.Stdout}, nil
    case "stderr":
        return nopCloser{os.Stderr}, nil
    default:
        return os.Create(dest)
    }
}
```

### 3. Profiling Integration in `main()`

Add profiling setup at start of execution, before `runFile()` or REPL:

```go
func main() {
    // ... existing flag parsing ...

    // Setup CPU profiling (applies to entire execution)
    var cpuWriter io.WriteCloser
    if opts.ProfileCPU != "" {
        var err error
        cpuWriter, err = resolveProfileWriter(opts.ProfileCPU)
        if err != nil {
            Failf(ExitCodeCannotCreate, "cannot create CPU profile output: %v", err)
        }
        if err := pprof.StartCPUProfile(cpuWriter); err != nil {
            cpuWriter.Close()
            Failf(ExitCodeCannotCreate, "cannot start CPU profile: %v", err)
        }
        defer func() {
            pprof.StopCPUProfile()
            cpuWriter.Close()
        }()
    }

    // Defer snapshot profile writes (heap, allocs, mutex) to end
    defer writeSnapshotProfiles()

    // ... existing file or REPL execution ...
}
```

### 4. Snapshot Profile Writer

```go
func writeSnapshotProfiles() {
    if opts.ProfileHeap != "" {
        w, err := resolveProfileWriter(opts.ProfileHeap)
        if err != nil {
            fmt.Fprintf(os.Stderr, "cannot create heap profile output: %v\n", err)
            return
        }
        defer w.Close()
        if err := pprof.WriteHeapProfile(w); err != nil {
            fmt.Fprintf(os.Stderr, "cannot write heap profile: %v\n", err)
        }
    }
    if opts.ProfileAllocs != "" {
        w, err := resolveProfileWriter(opts.ProfileAllocs)
        if err != nil {
            fmt.Fprintf(os.Stderr, "cannot create allocs profile output: %v\n", err)
            return
        }
        defer w.Close()
        if err := pprof.Lookup("allocs").WriteTo(w, 0); err != nil {
            fmt.Fprintf(os.Stderr, "cannot write allocs profile: %v\n", err)
        }
    }
    if opts.ProfileMutex != "" {
        runtime.SetMutexProfileFraction(1) // Enable mutex profiling
        w, err := resolveProfileWriter(opts.ProfileMutex)
        if err != nil {
            fmt.Fprintf(os.Stderr, "cannot create mutex profile output: %v\n", err)
            return
        }
        defer w.Close()
        if err := pprof.Lookup("mutex").WriteTo(w, 0); err != nil {
            fmt.Fprintf(os.Stderr, "cannot write mutex profile: %v\n", err)
        }
    }
}
```

### 5. Early Mutex Profiling Setup

For mutex profiling to capture data, it must be enabled before execution. Add at the start of `main()`:

```go
func main() {
    // Enable mutex profiling early if requested (must be set before locks are used)
    if opts.ProfileMutex != "" {
        runtime.SetMutexProfileFraction(1)
    }
    // ... rest of main ...
}
```

### 6. Makefile Targets (`go/Makefile`)

Add benchmark and profiling targets:

```makefile
.PHONY: bench bench-cpu bench-mem

bench:
	$(GO_TEST) -bench=. -benchmem ./...

bench-cpu:
	$(GO_TEST) -bench=. -cpuprofile=cpu.prof ./machine/...

bench-mem:
	$(GO_TEST) -bench=. -memprofile=mem.prof ./machine/...
```

## Files to Modify

| File | Changes |
|------|---------|
| `go/cmd/main.go` | Add profile flags, helper functions, profiling hooks |
| `go/Makefile` | Add bench targets |

## Usage Examples

### CPU profile to stderr (for logging)

```bash
./dist/scheme --profile-cpu=stderr -f benchmark.scm 2> cpu.prof
go tool pprof cpu.prof
```

### Heap profile to file

```bash
./dist/scheme --profile-heap=heap.prof -f memory-test.scm
go tool pprof -top heap.prof
```

### Multiple profiles

```bash
./dist/scheme --profile-cpu=cpu.prof --profile-heap=heap.prof -f test.scm
```

### Profile REPL session

```bash
./dist/scheme --profile-cpu=stderr 2> repl-cpu.prof
# ... interactive session ...
# Ctrl-D to exit and write profile
```

### Analyze with pprof

```bash
# Top functions by CPU time
go tool pprof -top cpu.prof

# Interactive web UI
go tool pprof -http=:8080 cpu.prof

# Generate flame graph SVG
go tool pprof -svg cpu.prof > cpu.svg
```

## Implementation Notes

1. **CPU profiling is process-global**: Only one CPU profile can be active. The Go runtime enforces this.

2. **Heap/allocs/mutex are snapshots**: Written at program exit, capturing state at that moment.

3. **Mutex profiling requires early setup**: `runtime.SetMutexProfileFraction(1)` must be called before locks are acquired.

4. **Profile format**: Raw pprof protobuf format, compatible with `go tool pprof`.

5. **stdout/stderr for logging**: Allows profiles to be captured via shell redirection to whatever logging system is in use.

## Verification

1. Build: `cd go && make build`
2. Test CPU profiling:
   ```bash
   ./dist/scheme --profile-cpu=stderr -f test.scm 2> cpu.prof
   go tool pprof -top cpu.prof
   ```
3. Test heap profiling:
   ```bash
   ./dist/scheme --profile-heap=heap.prof -f test.scm
   go tool pprof -top heap.prof
   ```
4. Test multiple profiles:
   ```bash
   ./dist/scheme --profile-cpu=cpu.prof --profile-allocs=allocs.prof -f test.scm
   ```
5. Run tests: `cd go && make test`
6. Verify REPL profiling works (Ctrl-D to exit and flush profile)
