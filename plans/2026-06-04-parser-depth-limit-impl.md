# Parser Depth-Limit Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Bound structural nesting depth in the parser so adversarial input (e.g. millions of nested `(`) returns a catchable `ParserError` instead of crashing the host process with a fatal, unrecoverable Go stack overflow.

**Architecture:** The recursive-descent parser funnels *all* nesting through one method, `(*Parser).readSyntax` (`internal/parser/parser.go:661`) — every compound reader (`readList`, `readVector`, `readByteVector`, `readLabeledList`, `readQuoteForm`, and the datum-comment branch) is dispatched from it and calls back into it. We add a depth counter to the `Parser` struct, increment/decrement it around the body of `readSyntax` (the single chokepoint), and return a new `werr.ErrParseDepthExceeded`-wrapped `ParserError` once a configurable bound is crossed. The bound is set to a safe default in the constructor (so every parser is protected with zero call-site changes) and is overridable via a setter and an optional engine option, mirroring the VM's existing `DefaultMaxCallDepth` / `WithMaxCallDepth` / `SetMaxCallDepth` pattern exactly. `0` means unlimited.

**Tech Stack:** Go, Wile parser (`internal/parser`), `werr` sentinel errors.

---

## Background: why this is needed

Verified live (audit, 2026-06-04): feeding `strings.Repeat("(", 2_000_000)` to the engine produces:

```
runtime: goroutine stack exceeds 1000000000-byte limit
fatal error: stack overflow
```

A Go `fatal error` is **not** recoverable by `recover()`, so an embedding host dies. This is reachable from untrusted Scheme text **even under the Console sandbox**, because parsing happens before any authorizer gate. The VM's `DefaultMaxCallDepth` (10000) guards *runtime* recursion but runs *after* parsing — it does nothing for parse-time nesting.

## Design decisions (surfaced for review)

1. **Single chokepoint.** Instrument `readSyntax` only. Every recursion path passes through it (confirmed: `readList`→`readSyntax`, `readVector`→`readSyntax`, `readByteVector`→`readSyntax`, `readLabeledList`→`readSyntax`, `readQuoteForm`→`readSyntax`, datum-comment→`readSyntax`). Atoms hit `readSyntax` at the current depth and return immediately, so a flat 10⁶-element list never grows depth — only *nesting* does. "Check once" per CLAUDE.md.
2. **Safe-by-default in the constructor.** The parser is built at ~8 sites (`engine.go:411,799`, `expression.go:88,116`, `extensions/eval/prim_eval.go:150`, `internal/extensions/io/prim_read_write.go:218,286`, `internal/bootstrap/bootstrap.go:316`). Threading a parameter through every `NewParser`/`NewParserWithFile` signature is invasive. Instead the constructor sets `maxDepth = DefaultMaxParseDepth`; all sites are protected with no signature change. A `SetMaxDepth` setter handles overrides. (Task 5 wires an engine option onto the two engine sites.)
3. **New sentinel, not reuse.** `werr.ErrCallDepthExceeded` means *runtime continuation depth*; parse nesting is a different family. A consumer may legitimately want to distinguish "your input is too deeply nested" (fix the input) from "your program recursed too deep at runtime" (fix the program). Add `werr.ErrParseDepthExceeded`. (Per the error-classification discipline: a new sentinel only when no existing one fits — it doesn't here.)
4. **Default = 10000, `0` = unlimited.** Mirrors `DefaultMaxCallDepth`. Each nesting level costs ~2 Go frames (`readList`+`readSyntax`), so 10000 levels ≈ low tens of MB — far under Go's 1 GB stack ceiling, yet 10000 levels of literal nesting is far beyond legitimate hand-written or normally-generated Scheme. **Trade-off:** a caller with genuinely deeper machine-generated data must opt out via `SetMaxDepth(0)` / `WithMaxParseDepth(0)`. This matches the VM's stance and R7RS (which mandates no limit).

## Out of scope (follow-up)

The **expander** (`machine/expander_*.go`) is also recursive and can overflow on deeply-nested *valid* forms. Bounding the parser closes the textual-input attack surface completely (deeply-nested text now fails to parse before it can reach the expander). Programmatically-constructed deep syntax (macro output, `datum->syntax`, quasiquote) can still reach the expander deeply nested; that is a separate, lower-severity hardening task and is **not** covered here. Note it in `TODO.md` when this lands.

## File Structure

| File | Change | Responsibility |
|------|--------|----------------|
| `werr/werr.go` | Modify (add 1 sentinel) | Declare `ErrParseDepthExceeded` |
| `internal/parser/parser.go` | Modify | Add `depth`/`maxDepth` fields, `DefaultMaxParseDepth` const, constructor default, `SetMaxDepth`, and the guard in `readSyntax` |
| `internal/parser/parser_depth_test.go` | Create | Unit tests: limit trips on each nesting form; valid nesting passes; boundary; multi-expression resets; `0` = unlimited |
| `options.go` | Modify (Task 5, optional) | `WithMaxParseDepth` engine option |
| `engine.go` | Modify (Task 5, optional) | `engineConfig.maxParseDepth` field + thread `SetMaxDepth` onto the two engine parser sites |
| `engine_parse_depth_test.go` | Create (Task 5) | Integration regression: `EvalMultiple` on deep input returns an error, does not crash |
| `TODO.md` | Modify | Record the expander follow-up |

---

### Task 1: Add the `ErrParseDepthExceeded` sentinel

**Files:**
- Modify: `werr/werr.go` (the "Recursion depth" sentinel group, ~line 156)

- [ ] **Step 1: Add the sentinel**

In `werr/werr.go`, in the `var (...)` block next to `ErrCallDepthExceeded`:

```go
	// Recursion depth
	ErrCallDepthExceeded     = NewStaticError("call depth exceeded")
	ErrParseDepthExceeded    = NewStaticError("parse nesting depth exceeded")
	ErrContinuationUnderflow = NewStaticError("continuation underflow")
```

- [ ] **Step 2: Verify it compiles**

Run: `go build ./werr/...`
Expected: exit 0, no output.

- [ ] **Step 3: Commit**

```bash
git add werr/werr.go
git commit -m "feat(werr): add ErrParseDepthExceeded sentinel"
```

---

### Task 2: Add the depth guard to the parser (TDD)

**Files:**
- Test: `internal/parser/parser_depth_test.go` (create)
- Modify: `internal/parser/parser.go` (struct ~line 50, constructor ~line 68, new const + setter, guard at `readSyntax` ~line 661)

- [ ] **Step 1: Write the failing test**

Create `internal/parser/parser_depth_test.go`:

```go
package parser

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/werr"
)

// Deeply nested parens must return a catchable ErrParseDepthExceeded,
// never crash with a fatal Go stack overflow.
func TestParser_DepthLimit_Trips(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	// Well past DefaultMaxParseDepth (10000) but tiny in memory.
	src := strings.Repeat("(", 50000)
	p := NewParser(env, true, strings.NewReader(src))
	_, err := p.ReadSyntax(context.TODO())
	if err == nil {
		t.Fatal("expected depth-limit error, got nil")
	}
	if !errors.Is(err, werr.ErrParseDepthExceeded) {
		t.Fatalf("expected ErrParseDepthExceeded, got: %v", err)
	}
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `go test ./internal/parser/ -run TestParser_DepthLimit_Trips -v`
Expected: FAIL — either a `fatal error: stack overflow` (process dies) or `undefined: werr.ErrParseDepthExceeded` if Task 1 was skipped. (Task 1 must be done first.)

- [ ] **Step 3: Add the `DefaultMaxParseDepth` const**

In `internal/parser/parser.go`, just above `type Parser struct` (~line 49):

```go
// DefaultMaxParseDepth bounds structural nesting depth during parsing.
// Without a bound, adversarial input such as deeply nested parentheses
// triggers a fatal, unrecoverable Go stack overflow that kills the host
// process. 0 means unlimited. Mirrors the VM's DefaultMaxCallDepth.
const DefaultMaxParseDepth int = 10000
```

- [ ] **Step 4: Add the `depth` and `maxDepth` fields**

In the `Parser` struct (`internal/parser/parser.go:50`), append after `datumLabels`:

```go
	datumLabels map[int]syntax.SyntaxValue // R7RS §2.4 datum labels (#n= and #n#)
	depth       int                        // current structural nesting depth
	maxDepth    int                        // max nesting depth; 0 = unlimited
```

- [ ] **Step 5: Set the default in the constructor**

In `NewParserWithFile` (`internal/parser/parser.go:68`):

```go
func NewParserWithFile(env *environment.EnvironmentFrame, skipComments bool, rdr io.RuneReader, file string) *Parser {
	q := &Parser{
		env:         env,
		rdr:         rdr,
		skipComment: skipComments,
		file:        file,
		maxDepth:    DefaultMaxParseDepth,
	}
	return q
}
```

- [ ] **Step 6: Add the `SetMaxDepth` setter**

Immediately after `NewParserWithFile` (before `func (p *Parser) curr()`):

```go
// SetMaxDepth sets the maximum structural nesting depth allowed during
// parsing. A value of 0 (or negative, clamped to 0) disables the limit.
// Mirrors MachineContext.SetMaxCallDepth.
func (p *Parser) SetMaxDepth(n int) {
	if n < 0 {
		n = 0
	}
	p.maxDepth = n
}
```

- [ ] **Step 7: Add the guard at the top of `readSyntax`**

In `readSyntax` (`internal/parser/parser.go:661`), insert the guard as the very first statements, before `var q syntax.SyntaxValue`:

```go
func (p *Parser) readSyntax() (syntax.SyntaxValue, tokenizer.Token, error) {
	p.depth++
	defer func() {
		p.depth--
	}()
	if p.maxDepth > 0 && p.depth > p.maxDepth {
		p.err = NewParserErrorWithWrapf(werr.ErrParseDepthExceeded, p.cur,
			"nesting depth exceeds maximum of %d", p.maxDepth)
		return nil, p.cur, p.err
	}

	var q syntax.SyntaxValue

	// Skip comments when skipComment is enabled
	// ... existing body unchanged ...
```

(`werr` is already imported in `parser.go`. The `defer` closure is intentionally multi-line per the project's no-single-line-function rule.)

- [ ] **Step 8: Run the test to verify it passes**

Run: `go test ./internal/parser/ -run TestParser_DepthLimit_Trips -v`
Expected: PASS.

- [ ] **Step 9: Run the full parser suite to verify no regression**

Run: `go test ./internal/parser/...`
Expected: `ok  github.com/aalpar/wile/internal/parser`.

- [ ] **Step 10: Commit**

```bash
git add internal/parser/parser.go internal/parser/parser_depth_test.go
git commit -m "fix(parser): bound nesting depth to prevent stack-overflow crash

Adversarial deeply nested input (e.g. millions of nested parens) caused a
fatal, unrecoverable Go stack overflow that killed the host process, even
under the Console sandbox (parsing precedes authorization). Add a depth
counter at the single recursion chokepoint (readSyntax) returning a
catchable ErrParseDepthExceeded past DefaultMaxParseDepth (10000).
0 disables the limit. Mirrors the VM's MaxCallDepth pattern."
```

---

### Task 3: Cover every nesting form and the boundary

**Files:**
- Modify: `internal/parser/parser_depth_test.go`

- [ ] **Step 1: Add the table-driven and boundary tests**

Append to `internal/parser/parser_depth_test.go`:

```go
// Each compound form recurses through readSyntax and must be bounded.
func TestParser_DepthLimit_AllForms(t *testing.T) {
	cases := []struct {
		name   string
		prefix string // repeated to build nesting
	}{
		{"list", "("},
		{"bracket", "["},
		{"vector", "#("},
		{"quote", "'"},
		{"quasiquote", "`"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			src := strings.Repeat(tc.prefix, 50000)
			p := NewParser(env, true, strings.NewReader(src))
			_, err := p.ReadSyntax(context.TODO())
			if !errors.Is(err, werr.ErrParseDepthExceeded) {
				t.Fatalf("%s: expected ErrParseDepthExceeded, got: %v", tc.name, err)
			}
		})
	}
}

// Nesting within the limit must parse without error.
func TestParser_DepthLimit_WithinLimitOK(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	depth := 1000 // well under 10000
	src := strings.Repeat("(", depth) + "1" + strings.Repeat(")", depth)
	p := NewParser(env, true, strings.NewReader(src))
	q, err := p.ReadSyntax(context.TODO())
	if err != nil {
		t.Fatalf("valid depth-%d nesting should parse, got: %v", depth, err)
	}
	if q == nil {
		t.Fatal("expected a syntax value, got nil")
	}
}

// The limit is configurable; SetMaxDepth(0) disables it for callers with
// legitimately deep machine-generated data (bounded here so the test is cheap).
func TestParser_DepthLimit_Configurable(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	src := strings.Repeat("(", 50) + "1" + strings.Repeat(")", 50)

	// A tight limit trips.
	p := NewParser(env, true, strings.NewReader(src))
	p.SetMaxDepth(10)
	if _, err := p.ReadSyntax(context.TODO()); !errors.Is(err, werr.ErrParseDepthExceeded) {
		t.Fatalf("tight limit should trip, got: %v", err)
	}

	// SetMaxDepth(0) disables the check.
	p2 := NewParser(env, true, strings.NewReader(src))
	p2.SetMaxDepth(0)
	if _, err := p2.ReadSyntax(context.TODO()); err != nil {
		t.Fatalf("disabled limit should parse, got: %v", err)
	}
}

// Depth must reset between successive top-level reads, so a long stream of
// shallow expressions does not accumulate depth and falsely trip.
func TestParser_DepthLimit_ResetsBetweenReads(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	src := strings.Repeat("(a) ", 20000) // 20000 shallow forms, depth never exceeds ~2
	p := NewParser(env, true, strings.NewReader(src))
	for i := 0; i < 20000; i++ {
		if _, err := p.ReadSyntax(context.TODO()); err != nil {
			t.Fatalf("read %d should succeed (depth must reset), got: %v", i, err)
		}
	}
}
```

- [ ] **Step 2: Run the tests**

Run: `go test ./internal/parser/ -run TestParser_DepthLimit -v`
Expected: PASS (all four). `ResetsBetweenReads` specifically guards the `defer p.depth--` correctness — if decrement were missing, depth would accumulate across the 20000 reads and falsely trip.

- [ ] **Step 3: Commit**

```bash
git add internal/parser/parser_depth_test.go
git commit -m "test(parser): cover depth limit across forms, boundary, reset, config"
```

---

### Task 4: Engine-level regression test (the security guarantee)

**Files:**
- Create: `engine_parse_depth_test.go` (root `wile` package, `package wile_test`)

- [ ] **Step 1: Write the integration test**

Create `engine_parse_depth_test.go`:

```go
package wile_test

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile"
)

// Regression for the audit finding: untrusted deeply-nested text fed to the
// public API must return an error, NOT crash the host with a fatal stack
// overflow. If the bound regressed, this test binary would die rather than fail.
func TestEngine_DeepNesting_DoesNotCrash(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}
	src := strings.Repeat("(", 2_000_000) // crashed the process pre-fix
	_, err = engine.EvalMultiple(ctx, src)
	if err == nil {
		t.Fatal("expected an error for pathologically nested input, got nil")
	}
	t.Logf("got expected error: %v", err)
}
```

- [ ] **Step 2: Run the test**

Run: `go test . -run TestEngine_DeepNesting_DoesNotCrash -v`
Expected: PASS, with a logged error mentioning nesting depth. (Pre-fix, this would print `fatal error: stack overflow` and the test binary would abort.)

- [ ] **Step 3: Commit**

```bash
git add engine_parse_depth_test.go
git commit -m "test(engine): regression — deep nested input errors, not crashes"
```

---

### Task 5 (optional): Expose `WithMaxParseDepth` engine option

Only do this if a configurable engine-level knob is wanted. The safety fix is already complete after Task 2 (constructor default protects all sites). This task threads the option onto the two engine-owned parser sites.

**Files:**
- Modify: `options.go` (after `WithMaxCallDepth`, ~line 158)
- Modify: `engine.go` (`engineConfig` struct; the two `parser.New*` sites at `:411` and `:799`)
- Test: `engine_parse_depth_test.go`

- [ ] **Step 1: Add the option**

In `options.go`, after `WithMaxCallDepth`:

```go
// WithMaxParseDepth sets the maximum structural nesting depth the parser
// will accept. When input nests deeper, ErrParseDepthExceeded is returned
// instead of crashing with a fatal Go stack overflow. A value of 0 means
// unlimited (negative values are clamped to 0). When not called, the parser
// uses DefaultMaxParseDepth (10000).
func WithMaxParseDepth(n int) EngineOption {
	return func(cfg *engineConfig) {
		if n < 0 {
			n = 0
		}
		cfg.maxParseDepth = n
		cfg.parseDepthSet = true
	}
}
```

- [ ] **Step 2: Add config fields and default**

In `engine.go`, add to `engineConfig` (near `maxCallDepth`/`callDepthSet`):

```go
	maxParseDepth int
	parseDepthSet bool
```

Where `maxCallDepth` is defaulted in `NewEngine` (`engine.go:200-201`), add the parallel default:

```go
	if !cfg.parseDepthSet {
		cfg.maxParseDepth = parser.DefaultMaxParseDepth
	}
```

Add `maxParseDepth int` to the `Engine` struct and copy it through in `NewEngine` (alongside `maxCallDepth: cfg.maxCallDepth`):

```go
		maxParseDepth: cfg.maxParseDepth,
```

- [ ] **Step 3: Thread it onto the engine parser sites**

At `engine.go:411` and `engine.go:799`, after constructing the parser, call the setter. Example at :411:

```go
	pr := parser.NewParserWithFile(p.env, true, reader, source)
	pr.SetMaxDepth(p.maxParseDepth)
```

And at :799:

```go
		pr := parser.NewParser(env, true, reader)
		pr.SetMaxDepth(p.maxParseDepth)
```

- [ ] **Step 4: Add the option test**

Append to `engine_parse_depth_test.go`:

```go
func TestEngine_WithMaxParseDepth(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx, wile.WithMaxParseDepth(20))
	if err != nil {
		t.Fatal(err)
	}
	src := strings.Repeat("(", 100) + "1" + strings.Repeat(")", 100)
	if _, err := engine.EvalMultiple(ctx, src); err == nil {
		t.Fatal("depth 100 under a limit of 20 should error")
	}
}
```

- [ ] **Step 5: Run the tests**

Run: `go test . -run TestEngine_ -v`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add options.go engine.go engine_parse_depth_test.go
git commit -m "feat(engine): WithMaxParseDepth to configure parser nesting bound"
```

---

### Task 6: Record the expander follow-up

**Files:**
- Modify: `TODO.md`

- [ ] **Step 1: Add a TODO entry**

Add under the appropriate section of `TODO.md`:

```markdown
- [ ] Bound expander recursion depth (machine/expander_*.go). The parser depth
      limit (plans/2026-06-04-parser-depth-limit-impl.md) closes the textual-input
      stack-overflow surface, but programmatically-constructed deep syntax
      (macro output, datum->syntax, quasiquote) can still overflow the expander.
```

- [ ] **Step 2: Commit**

```bash
git add TODO.md
git commit -m "docs(todo): note expander depth-bound follow-up"
```

---

## Final verification

- [ ] `go build ./...` → exit 0
- [ ] `go vet ./...` → clean
- [ ] `make lint` → 0 issues
- [ ] `go test ./internal/parser/... .` → pass
- [ ] Manual: `printf '%.0s(' $(seq 1 2000000) > /tmp/nest.scm && go run ./cmd/wile --file /tmp/nest.scm` → prints a parse error, exits non-zero, **no** `fatal error: stack overflow`

## Self-review notes

- **Spec coverage:** crash-prevention (Task 2 guard + Task 4 regression), every nesting form (Task 3), boundary + configurability + depth-reset (Task 3), engine knob (Task 5), expander scoped out with rationale (Task 6).
- **Type consistency:** `maxDepth`/`depth` (parser fields), `DefaultMaxParseDepth` (parser const), `SetMaxDepth` (parser method), `werr.ErrParseDepthExceeded` (sentinel), `WithMaxParseDepth`/`maxParseDepth`/`parseDepthSet` (engine) — names used identically across all tasks.
- **No placeholders:** every code step shows complete code and exact run/expected commands.
