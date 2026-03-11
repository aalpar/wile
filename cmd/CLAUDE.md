# cmd/ — CLI and Binary

## GNU Flag Conventions

Wile follows standard GNU command-line option conventions. All flags use the `go-flags` library (`github.com/jessevdk/go-flags`).

### Rules

- **Short flags**: Single dash, single letter: `-f`, `-v`, `-V`
- **Long flags**: Double dash, full word: `--file`, `--verbose`, `--version`
- **NEVER** use single-dash long flags (e.g., `-file` is wrong, `--file` is correct)
- Short and long forms are aliases for the same option
- Boolean flags don't take arguments (`--verbose`, not `--verbose=true`)
- Value flags use `--flag VALUE` or `--flag=VALUE` or `-f VALUE`
- `--` terminates flag parsing; everything after is a positional argument

### Current Flags

| Short | Long | Type | Description |
|-------|------|------|-------------|
| `-e` | `--eval` | []string | Evaluate Scheme expression (repeatable) |
| `-f` | `--file` | []string | Scheme file(s) to load (repeatable) |
| `-i` | `--interactive` | bool | Enter REPL after loading file(s) |
| `-L` | `--library-path` | string | Library search paths (colon-separated) |
| `-q` | `--quiet` | bool | Suppress informational messages |
| `-V` | `--version` | bool | Print version and exit |

### Reserved Short Flags (GNU Convention)

When adding new flags, prefer these standard GNU short-flag assignments:

| Short | Long | Convention |
|-------|------|------------|
| `-v` | `--verbose` | Increase verbosity |
| `-q` | `--quiet` / `--silent` | Suppress output |
| `-h` | `--help` | Show help (handled automatically by `go-flags`) |
| `-o` | `--output` | Output file |
| `-d` | `--debug` | Enable debug mode |
| `-n` | `--dry-run` | Show what would be done |

## Build Commands

```bash
make build            # Build to ./dist/{os}/{arch}/wile (e.g., dist/darwin/arm64/wile)
make build-all        # Build for all platforms (darwin/linux x arm64/amd64)
make test             # Run all tests (go test -v ./...)
make lint             # Run golangci-lint
make fix              # Run golangci-lint with --fix
make cover            # Run tests with coverage
make format           # Format code with golangci-lint
make tidy             # Tidy go.mod
make release-check    # Validate .goreleaser.yml syntax
make release-snapshot # Dry-run release build (no publish, output in dist/)
make release          # Full GoReleaser release (requires tag context)
```

Quick build (convenience binary at dist root):
```bash
go build -o dist/wile ./cmd/wile
```

Run a single test:
```bash
go test -v -run TestName ./package/...
```

Run the REPL:
```bash
./dist/wile                            # Using convenience binary
./dist/darwin/arm64/wile               # Using platform-specific binary
```

Run a Scheme file:
```bash
./dist/wile --file example.scm
```

### dist/ Directory Structure

```
dist/
├── scheme                    # Convenience binary (from go build -o dist/wile)
├── darwin/
│   ├── arm64/
│   │   └── scheme            # macOS ARM64 binary (from make build on M1/M2)
│   └── amd64/
│       └── scheme            # macOS Intel binary
└── linux/
    ├── arm64/
    │   └── scheme            # Linux ARM64 binary
    └── amd64/
        └── scheme            # Linux Intel binary
```
