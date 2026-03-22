# Expression Type API Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace string-based single-expression APIs with `*Expression`-based APIs, eliminating silent partial consumption by construction.

**Architecture:** New `Expression` type wraps `syntax.SyntaxValue` (same pattern as `CompiledCode` wrapping `NativeTemplate`). New `Parse`/`ParseWithSource`/`MustParse`/`MustParseWithSource` methods produce `*Expression`. Existing `EvalMultiple`/`EvalMultipleWithSource` unchanged.

**Tech Stack:** Pure Go, no new dependencies.

---

## Task 1: Create Expression type and Parse methods

**Files:**
- Create: `expression.go`
- Modify: `engine_unit_test.go`

**Step 1: Write the failing test**

Add to `engine_unit_test.go`:

```go
func TestEngineParse(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	t.Run("single expression", func(t *testing.T) {
		expr, err := engine.Parse(ctx, "(+ 1 2)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, expr, qt.IsNotNil)
	})

	t.Run("trailing input errors", func(t *testing.T) {
		_, err := engine.Parse(ctx, "(+ 1 2) (+ 3 4)")
		qt.Assert(t, err, qt.IsNotNil)
		var compErr *wile.CompilationError
		qt.Assert(t, errors.As(err, &compErr), qt.IsTrue)
		qt.Assert(t, compErr.Message, qt.Matches, "trailing input.*")
	})

	t.Run("empty input errors", func(t *testing.T) {
		_, err := engine.Parse(ctx, "")
		qt.Assert(t, err, qt.IsNotNil)
		var compErr *wile.CompilationError
		qt.Assert(t, errors.As(err, &compErr), qt.IsTrue)
	})

	t.Run("parse error", func(t *testing.T) {
		_, err := engine.Parse(ctx, "(")
		qt.Assert(t, err, qt.IsNotNil)
		var compErr *wile.CompilationError
		qt.Assert(t, errors.As(err, &compErr), qt.IsTrue)
	})
}

func TestEngineParseWithSource(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	expr, err := engine.ParseWithSource(ctx, "(+ 1 2)", "test.scm")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expr, qt.IsNotNil)
	qt.Assert(t, expr.Source(), qt.Equals, "test.scm")
}

func TestEngineMustParse(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	t.Run("valid input", func(t *testing.T) {
		expr := engine.MustParse(ctx, "(+ 1 2)")
		qt.Assert(t, expr, qt.IsNotNil)
	})

	t.Run("invalid input panics", func(t *testing.T) {
		qt.Assert(t, func() {
			engine.MustParse(ctx, "(")
		}, qt.PanicMatches, ".*parse error.*")
	})

	t.Run("trailing input panics", func(t *testing.T) {
		qt.Assert(t, func() {
			engine.MustParse(ctx, "(+ 1 2) (+ 3 4)")
		}, qt.PanicMatches, ".*trailing input.*")
	})
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestEngineParse|TestEngineMustParse' .`
Expected: FAIL (Parse, ParseWithSource, MustParse, Source not defined)

**Step 3: Create `expression.go`**

```go
package wile

import (
	"context"
	"strings"

	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
)

// Expression represents a parsed Scheme expression ready for compilation
// or evaluation. It is produced by [Engine.Parse] and consumed by
// [Engine.Compile] and the single-expression [Engine.Eval].
//
// The "exactly one expression" constraint is enforced at parse time.
type Expression struct {
	stx    syntax.SyntaxValue
	source string
}

// Source returns the source attribution (filename) for this expression,
// or the empty string if none was provided.
func (p *Expression) Source() string {
	return p.source
}

// String returns a display representation.
func (p *Expression) String() string {
	return "#<expression>"
}

// Parse parses a single Scheme expression from code.
// Returns a [CompilationError] if code is empty, malformed, or contains
// more than one expression. For multi-expression input, use
// [Engine.EvalMultiple].
func (p *Engine) Parse(ctx context.Context, code string) (*Expression, error) {
	return p.parse(ctx, code, "")
}

// ParseWithSource is like [Engine.Parse] but associates a source name
// (typically a filename) with the expression for error messages and
// stack traces.
func (p *Engine) ParseWithSource(ctx context.Context, code string, source string) (*Expression, error) {
	return p.parse(ctx, code, source)
}

// MustParse is like [Engine.Parse] but panics on error.
// Use for known-good string literals in tests and examples.
func (p *Engine) MustParse(ctx context.Context, code string) *Expression {
	expr, err := p.Parse(ctx, code)
	if err != nil {
		panic(err)
	}
	return expr
}

// MustParseWithSource is like [Engine.ParseWithSource] but panics on error.
func (p *Engine) MustParseWithSource(ctx context.Context, code string, source string) *Expression {
	expr, err := p.ParseWithSource(ctx, code, source)
	if err != nil {
		panic(err)
	}
	return expr
}

func (p *Engine) parse(ctx context.Context, code string, source string) (*Expression, error) {
	reader := strings.NewReader(code)
	pr := parser.NewParserWithFile(p.env, true, reader, source)

	stx, err := pr.ReadSyntax(ctx)
	if err != nil {
		return nil, &CompilationError{Message: "parse error", Cause: err}
	}

	_, trailing := pr.ReadSyntax(ctx)
	if trailing == nil || !isEOF(trailing) {
		return nil, &CompilationError{
			Message: "trailing input after expression (use EvalMultiple for multiple expressions)",
		}
	}

	return &Expression{stx: stx, source: source}, nil
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -v -run 'TestEngineParse|TestEngineMustParse' .`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: 0 issues

**Step 6: Commit**

```
feat: add Expression type with Parse, ParseWithSource, MustParse
```

---

## Task 2: Change signatures of single-expression APIs

**Files:**
- Modify: `engine.go`
- Modify: `engine_unit_test.go`

**Step 1: Update tests to use new signatures**

In `engine_unit_test.go`:
- `TestEngineEval`: change `engine.Eval(ctx, tc.code)` to `engine.Eval(ctx, engine.MustParse(ctx, tc.code))`
- `TestEngineCompileAndRun`: change `engine.Compile(ctx, ...)` to `engine.Compile(ctx, engine.MustParse(ctx, ...))`
- Remove `TestEngineEvalTrailingInput` and `TestEngineCompileTrailingInput` (trailing input is now tested in `TestEngineParse`)
- For the "compile invalid syntax" test case: test Parse, not Compile, since parse errors now happen in Parse

**Step 2: Change method signatures in `engine.go`**

Replace methods at lines ~292-427:

```go
// Eval compiles and executes a parsed expression, returning the result.
// Use [Engine.Parse] to obtain an [Expression] from source code.
// For evaluating multi-expression strings, use [Engine.EvalMultiple].
func (p *Engine) Eval(ctx context.Context, expr *Expression) (Value, error) {
	compiled, err := p.Compile(ctx, expr)
	if err != nil {
		return nil, err
	}
	return p.Run(ctx, compiled)
}

// EvalIn compiles and executes a parsed expression in the given namespace.
//
// The target namespace's authorizer governs security checks during
// execution. If the target namespace has no authorizer, the engine's
// authorizer is propagated to it before evaluation.
func (p *Engine) EvalIn(ctx context.Context, expr *Expression, ns *environment.Namespace) (Value, error) {
	if ns.Authorizer() == nil && p.namespace.Authorizer() != nil {
		ns.SetAuthorizer(p.namespace.Authorizer())
	}
	env := ns.Runtime()

	tpl, err := expandAndCompile(ctx, env, expr.stx, nil)
	if err != nil {
		return nil, &CompilationError{Message: "expand/compile error", Cause: err}
	}

	cc := &CompiledCode{template: tpl, env: env}
	return p.runCompiled(ctx, cc)
}

// Compile compiles a parsed expression without executing.
// The result can be executed later with [Engine.Run].
func (p *Engine) Compile(ctx context.Context, expr *Expression) (*CompiledCode, error) {
	return p.compileExpr(ctx, expr.stx)
}
```

Remove these methods entirely:
- `EvalWithSource` (line ~339-348)
- `CompileWithSource` (line ~396-401)
- `compile()` private helper (line ~403-422)

Remove `strings` import if it becomes unused (check — `evalMultiple` still
uses `strings.NewReader`, so it stays).

**Step 3: Run affected tests**

Run: `go test -v -run 'TestEngineEval$|TestEngineCompileAndRun|TestEngine_EvalIn' .`
Expected: PASS

**Step 4: Commit**

```
feat!: single-expression APIs accept *Expression instead of string

BREAKING: Eval, Compile, EvalIn now take *Expression (from Parse).
EvalWithSource and CompileWithSource removed — source is on Expression.
```

---

## Task 3: Migrate wile root-package test files

Mechanical replacement across all root-package test files. ~116 `Eval` sites,
~10 `Compile` sites, ~6 `EvalWithSource`/`CompileWithSource` sites.

**Files:**
- `engine_sandbox_test.go`
- `wile_test.go`
- `wile_bench_test.go`
- `ffi_test.go`
- `example_test.go`
- `opcode_fusion_test.go`
- `fs_source_test.go`
- `with_source_test.go`
- `library_test.go`
- `peephole_census_test.go`

**Replacement rules:**

| Before | After |
|--------|-------|
| `engine.Eval(ctx, code)` | `engine.Eval(ctx, engine.MustParse(ctx, code))` |
| `engine.Compile(ctx, code)` | `engine.Compile(ctx, engine.MustParse(ctx, code))` |
| `engine.EvalWithSource(ctx, code, src)` | `engine.Eval(ctx, engine.MustParseWithSource(ctx, code, src))` |
| `engine.CompileWithSource(ctx, code, src)` | `engine.Compile(ctx, engine.MustParseWithSource(ctx, code, src))` |
| `eng.EvalIn(ctx, code, ns)` | `eng.EvalIn(ctx, eng.MustParse(ctx, code), ns)` |

**Special case — deliberate parse-error tests:**

Tests that call `engine.Eval(ctx, "(")` expecting a parse error should test
`engine.Parse(ctx, "(")` instead, since `MustParse` would panic.

Example from `wile_test.go`:
```go
// Before
_, err = engine.Eval(ctx, "(")
// After
_, err = engine.Parse(ctx, "(")
```

**Special case — CompileWithSource parse-error test:**

`with_source_test.go` line 244: `engine.CompileWithSource(ctx, "(", "broken.scm")`
becomes `engine.ParseWithSource(ctx, "(", "broken.scm")`.

**Step 1: Apply replacements to all files**

Work through each file applying the rules above.

**Step 2: Run full test suite**

Run: `go test ./...`
Expected: ALL PASS

**Step 3: Run lint**

Run: `make lint`
Expected: 0 issues

**Step 4: Commit**

```
refactor: migrate root-package tests to Expression-based API
```

---

## Task 4: Migrate extension and integration test files

**Files:**

Extension tests:
- `extensions/files/prim_files_test.go`
- `extensions/files/with_file_continuation_test.go`
- `extensions/gointerop/prim_gointerop_test.go`
- `extensions/introspection/prim_introspection_test.go`
- `extensions/math/prim_math_test.go`
- `extensions/system/prim_system_test.go`
- `extensions/threads/prim_threads_test.go`

Internal tests:
- `internal/extensions/all/prim_characters_test.go`
- `internal/extensions/eval/prim_eval_test.go` (direct calls; helper already uses EvalMultiple)
- `internal/extensions/eval/load_path_integration_test.go`
- `internal/extensions/io/prim_ports_test.go`
- `internal/extensions/io/prim_read_write_test.go`

Integration tests:
- `integration/callcc_callback_test.go`
- `integration/circular_test.go`
- `integration/cont_mark_test.go`
- `integration/quasisyntax_test.go`

**Same replacement rules as Task 3.**

For `evalExpectError` helper in `internal/extensions/eval/prim_eval_test.go`:

```go
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return // parse error counts as expected error
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.Assert(t, err, qt.IsNotNil)
}
```

**Step 1: Apply replacements**

**Step 2: Run full test suite**

Run: `go test ./...`
Expected: ALL PASS

**Step 3: Run lint**

Run: `make lint`
Expected: 0 issues

**Step 4: Commit**

```
refactor: migrate extension and integration tests to Expression-based API
```

---

## Task 5: Update examples and documentation

**Files:**
- Modify: `doc.go` (line 23)
- Modify: `README.md` (lines 16, 87, 101)
- Modify: `examples/embedding/basic.go` (lines 39, 56, 81, 110, 115)
- Modify: `examples/embedding/source-tracking/main.go` (lines 48, 84, 91, 125, 137)

**Step 1: Update `doc.go`**

Line 23:
```go
//	result, err := engine.Eval(ctx, engine.MustParse(ctx, "(+ 1 2 3)"))
```

**Step 2: Update `README.md`**

Line 16:
```go
result, _ := engine.Eval(ctx, engine.MustParse(ctx, "(* width height)"))
```

Lines 87-88:
```go
result, err := engine.Eval(ctx, engine.MustParse(ctx, "(+ 1 2 3)"))
```

Lines 101-102:
```go
compiled, err := engine.Compile(ctx, engine.MustParse(ctx, "(+ x 1)"))
```

**Step 3: Update `examples/embedding/basic.go`**

All `engine.Eval(ctx, code)` become `engine.Eval(ctx, engine.MustParse(ctx, code))`.
`engine.Compile(ctx, code)` becomes `engine.Compile(ctx, engine.MustParse(ctx, code))`.

**Step 4: Update `examples/embedding/source-tracking/main.go`**

All `engine.EvalWithSource(ctx, code, src)` become
`engine.Eval(ctx, engine.MustParseWithSource(ctx, code, src))`.
`engine.CompileWithSource(ctx, code, src)` becomes
`engine.Compile(ctx, engine.MustParseWithSource(ctx, code, src))`.

**Step 5: Verify examples compile**

Run: `go build ./examples/...`
Expected: success

**Step 6: Commit**

```
docs: update examples and documentation for Expression API
```

---

## Task 6: Migrate external consumers

**Files (wile-goast):**
- `goast/prim_goast_test.go` (eval/evalExpectError helpers)
- `goastssa/prim_ssa_test.go`
- `goastcg/prim_callgraph_test.go`
- `goastcfg/prim_cfg_test.go`
- `goastlint/prim_lint_test.go`

**Files (wile-extension-example):**
- `internal/display/display.go` (lines 19, 49)
- `cmd/ffi-callbacks/main.go` (lines 122, 129)

**Same replacement rules as previous tasks.**

For wile-goast test helpers (same pattern in all 5 files):
```go
// eval helper — switch to EvalMultiple (accepts arbitrary code)
func eval(...) {
	result, err := engine.EvalMultiple(ctx, code)
	...
}

// evalExpectError — handle parse errors too
func evalExpectError(...) {
	expr, err := engine.Parse(ctx, code)
	if err != nil {
		return
	}
	_, err = engine.Eval(ctx, expr)
	...
}
```

**Step 1: Apply replacements in wile-goast**

**Step 2: Apply replacements in wile-extension-example**

**Step 3: Run tests in both repos**

```bash
(cd ../wile-goast && go test ./...)
(cd ../wile-extension-example && go test ./...)
```
Expected: ALL PASS

**Step 4: Commit in each repo**

```
refactor: migrate to wile Expression-based API
```

---

## Task 7: Final cleanup and verification

**Files:**
- Modify: `engine.go` — verify no dead code remains
- Verify: no remaining callers of old signatures

**Step 1: Verify no remaining callers of old signatures**

```bash
grep -rn 'Eval(ctx,\s*"' --include='*.go' .
grep -rn 'Eval(ctx,\s*`' --include='*.go' .
grep -rn 'Compile(ctx,\s*"' --include='*.go' .
grep -rn 'EvalWithSource\|CompileWithSource' --include='*.go' .
```
Expected: no matches in non-comment code

**Step 2: Full test suite**

Run: `go test ./...`
Expected: ALL PASS

**Step 3: Lint and covercheck**

Run: `make lint && make covercheck`
Expected: both pass

**Step 4: Commit cleanup if needed**

```
chore: final cleanup for Expression API migration
```
