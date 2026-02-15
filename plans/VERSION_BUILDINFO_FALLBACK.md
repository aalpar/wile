# Plan: Version/SHA Fallback via debug.ReadBuildInfo

**Status**: Proposed
**Priority**: Low (cosmetic — affects `go install` users only)

---

## Problem

When installed via `go install github.com/aalpar/wile/cmd/scheme@v1.3.0`, the `--version` flag prints:

```
Wile Scheme  ()
```

The `BuildVersion` and `BuildSHA` vars are empty because `go install` doesn't inject `-ldflags`. Only `make build` and GoReleaser set them.

## Solution

Use `debug.ReadBuildInfo()` as a fallback when ldflags are empty. Go embeds module version and VCS info into every binary automatically.

### Available from ReadBuildInfo

| Field | Source | Example |
|-------|--------|---------|
| `Main.Version` | Module version | `v1.3.0` |
| `vcs.revision` setting | Git SHA | `4a82ce5...` (full 40-char) |
| `vcs.modified` setting | Dirty tree | `true` / `false` |

### Implementation

Single file change: `cmd/scheme/main.go`

```
func resolveVersion() (version, sha string) {
    version = BuildVersion
    sha = BuildSHA

    if version != "" && sha != "" {
        return
    }

    info, ok := debug.ReadBuildInfo()
    if !ok {
        return
    }

    if version == "" && info.Main.Version != "" && info.Main.Version != "(devel)" {
        version = info.Main.Version
    }

    if sha == "" {
        for _, s := range info.Settings {
            if s.Key == "vcs.revision" && len(s.Value) >= 7 {
                sha = s.Value[:7]
                break
            }
        }
    }

    return
}
```

Then change line 135:

```go
// Before
fmt.Printf("Wile Scheme %s (%s)\n", BuildVersion, BuildSHA)

// After
v, s := resolveVersion()
fmt.Printf("Wile Scheme %s (%s)\n", v, s)
```

### Priority order

1. ldflags values (Makefile / GoReleaser) — exact, controlled
2. `debug.ReadBuildInfo` — automatic, good enough
3. Empty — development builds from `go run`

### Expected results

| Install method | Before | After |
|----------------|--------|-------|
| `make build` | `v1.3.0 (96e1bae)` | `v1.3.0 (96e1bae)` (unchanged) |
| `go install ...@v1.3.0` | `()` | `v1.3.0 (4a82ce5)` |
| `go run ./cmd/scheme` | `()` | `(devel)` filtered → `()` (unchanged) |

### Test changes

Update `TestVersionOutput` to cover the fallback path.

## Scope

- 1 file changed: `cmd/scheme/main.go`
- 1 new import: `runtime/debug` (stdlib)
- ~15 lines of code
- No new dependencies
