# machine/compilation Coverage Improvement Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Bring `machine/compilation` from 68.6% to 80%+ statement coverage (424 more covered statements), then remove its exclusion from `covercheck.sh`.

**Architecture:** Mixed Scheme-level and white-box tests. A new `SetupEngineTest` helper in `machine/testutil/` wires up `FSFileResolver` + `LibraryRegistry` + `LibraryEnvFactory` to enable library loading in tests without importing the root `wile` package (avoiding circular deps). Scheme-level tests exercise happy paths through the full pipeline; white-box tests cover error branches.

**Tech Stack:** Go testing, `testing/fstest.MapFS`, `quicktest`, existing `testutil.EvalSchemeInEnv`

---

## Verification

After each phase, run:

```bash
go test -coverprofile=/tmp/compilation-cov.out ./machine/compilation/...
go tool cover -func=/tmp/compilation-cov.out | grep 'machine/compilation/' | tail -1
```

The final line shows `total:` coverage. Target: 80.0% or higher.

Final verification:

```bash
make lint && make covercheck
```

---

### Task 1: Add SetupEngineTest helper to machine/testutil

**Files:**
- Modify: `machine/testutil/testutil.go`

This helper creates an environment with full library loading support, mirroring
what `wile.NewEngine(WithSourceFS(...))` does internally. It accepts an `fs.FS`
so tests can use `fstest.MapFS` with inline `.sld` content.

**Step 1: Write the helper function**

Add to `machine/testutil/testutil.go`:

```go
// SetupEngineTest creates a test environment with full library loading support,
// mirroring what wile.NewEngine(WithSourceFS(...)) does internally. The fsys
// parameter provides the virtual filesystem for library resolution (use
// testing/fstest.MapFS for inline .sld content). Pass nil to skip FS setup.
//
// This avoids importing the root wile package (which would create a circular
// dependency) while providing the same library infrastructure.
func SetupEngineTest(t *testing.T, fsys fs.FS) *environment.EnvironmentFrame {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrameTiny(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	// Wire up library infrastructure (same as Engine internals).
	env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)
	reg := compilation.NewLibraryRegistry()
	env.SetLibraryRegistry(reg)

	if fsys != nil {
		resolver := compilation.NewFSFileResolver(fsys, env)
		env.SetFileResolver(resolver)
	}

	return env
}
```

Add `"io/fs"` to the imports.

**Step 2: Write a smoke test**

Add to a new file `machine/testutil/testutil_test.go`:

```go
package testutil_test

import (
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/machine/testutil"
	"github.com/aalpar/wile/values"
	qt "github.com/frankban/quicktest"
)

func TestSetupEngineTest_SmokePlainArithmetic(t *testing.T) {
	env := testutil.SetupEngineTest(t, nil)
	result := testutil.EvalSchemeInEnv(t, env, "(+ 1 2)")
	qt.Assert(t, result.SchemeString(), qt.Equals, "3")
}

func TestSetupEngineTest_LibraryImport(t *testing.T) {
	fs := fstest.MapFS{
		"test/greet.sld": &fstest.MapFile{
			Data: []byte(`(define-library (test greet)
  (export greeting)
  (begin (define greeting "hello")))`),
		},
	}
	env := testutil.SetupEngineTest(t, fs)
	result := testutil.EvalSchemeInEnv(t, env, `
		(import (test greet))
		greeting`)
	qt.Assert(t, result, qt.Equals, values.NewString("hello"))
}
```

**Step 3: Run tests**

```bash
go test -v ./machine/testutil/...
```

Expected: PASS.

**Step 4: Commit**

```
feat(testutil): add SetupEngineTest for library-enabled test environments
```

---

### Task 2: Library system Scheme-level tests

**Files:**
- Create: `machine/compilation/library_system_test.go`

Uses `testutil.SetupEngineTest` with `fstest.MapFS` to exercise the full
import pipeline: `LoadLibrary` -> `resolveImportSet` -> `ApplyToExports` ->
`CopyLibraryBindingsToEnv`.

**Step 1: Write the test file**

```go
package compilation_test

import (
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/machine/testutil"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestLibrarySystemImport exercises the full library import pipeline:
// LoadLibrary -> resolveImportSet -> ApplyToExports -> CopyLibraryBindingsToEnv.
//
// Source files: library_loader.go, library_bindings.go, compile_import.go,
// compile_library_forms.go, compile_time_continuation_library.go.
func TestLibrarySystemImport(t *testing.T) {
	// Minimal library: export-only (re-exports bootstrap binding).
	// Exercises: LoadLibrary, resolveImportSet, CopyLibraryBindingsToEnv.
	basicFS := fstest.MapFS{
		"test/basic.sld": &fstest.MapFile{Data: []byte(
			`(define-library (test basic)
			   (export my-add)
			   (import (scheme base))
			   (begin (define my-add +)))`,
		)},
	}

	// Library with begin body defining new functions.
	// Exercises: compileLibraryBegin, processLibraryDeclaration, processFormsWithLetrecSemantics.
	beginFS := fstest.MapFS{
		"test/math.sld": &fstest.MapFile{Data: []byte(
			`(define-library (test math)
			   (export double triple)
			   (import (scheme base))
			   (begin
			     (define (double x) (* x 2))
			     (define (triple x) (* x 3))))`,
		)},
	}

	// Library with export rename.
	// Exercises: processLibraryExport with rename spec.
	renameExportFS := fstest.MapFS{
		"test/renamed.sld": &fstest.MapFile{Data: []byte(
			`(define-library (test renamed)
			   (export (rename internal-fn public-fn))
			   (import (scheme base))
			   (begin (define (internal-fn) 99)))`,
		)},
	}

	// Two libraries: one imports the other.
	// Exercises: transitive library loading.
	transitiveFS := fstest.MapFS{
		"test/base-lib.sld": &fstest.MapFile{Data: []byte(
			`(define-library (test base-lib)
			   (export base-val)
			   (import (scheme base))
			   (begin (define base-val 10)))`,
		)},
		"test/derived.sld": &fstest.MapFile{Data: []byte(
			`(define-library (test derived)
			   (export derived-val)
			   (import (scheme base) (test base-lib))
			   (begin (define derived-val (+ base-val 5))))`,
		)},
	}

	tcs := []struct {
		name     string
		fs       fstest.MapFS
		code     string
		expected values.Value
	}{
		{
			name:     "import basic library",
			fs:       basicFS,
			code:     "(import (test basic)) (my-add 3 4)",
			expected: values.NewInteger(7),
		},
		{
			name:     "library with begin body",
			fs:       beginFS,
			code:     "(import (test math)) (double 21)",
			expected: values.NewInteger(42),
		},
		{
			name:     "library with begin body second export",
			fs:       beginFS,
			code:     "(import (test math)) (triple 10)",
			expected: values.NewInteger(30),
		},
		{
			name:     "library export rename",
			fs:       renameExportFS,
			code:     "(import (test renamed)) (public-fn)",
			expected: values.NewInteger(99),
		},
		{
			name:     "transitive library loading",
			fs:       transitiveFS,
			code:     "(import (test derived)) derived-val",
			expected: values.NewInteger(15),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := testutil.SetupEngineTest(t, tc.fs)
			result := testutil.EvalSchemeInEnv(t, env, tc.code)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

// TestLibrarySystemImportModifiers exercises import set modifiers:
// only, except, prefix, rename.
//
// Source files: library_bindings.go (ApplyToExports, resolveImportSet).
func TestLibrarySystemImportModifiers(t *testing.T) {
	fs := fstest.MapFS{
		"test/multi.sld": &fstest.MapFile{Data: []byte(
			`(define-library (test multi)
			   (export alpha beta gamma)
			   (import (scheme base))
			   (begin
			     (define alpha 1)
			     (define beta 2)
			     (define gamma 3)))`,
		)},
	}

	tcs := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "import only alpha",
			code:     "(import (only (test multi) alpha)) alpha",
			expected: values.NewInteger(1),
		},
		{
			name:     "import except gamma",
			code:     "(import (except (test multi) gamma)) (+ alpha beta)",
			expected: values.NewInteger(3),
		},
		{
			name:     "import prefix",
			code:     "(import (prefix (test multi) t:)) t:alpha",
			expected: values.NewInteger(1),
		},
		{
			name:     "import rename",
			code:     "(import (rename (test multi) (alpha a))) a",
			expected: values.NewInteger(1),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := testutil.SetupEngineTest(t, fs)
			result := testutil.EvalSchemeInEnv(t, env, tc.code)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

// TestLibrarySystemErrors exercises error paths in library loading.
//
// Source files: library_loader.go (LoadLibrary error branches).
func TestLibrarySystemErrors(t *testing.T) {
	// Circular dependency: A imports B, B imports A.
	circularFS := fstest.MapFS{
		"test/circ-a.sld": &fstest.MapFile{Data: []byte(
			`(define-library (test circ-a)
			   (export a-val)
			   (import (scheme base) (test circ-b))
			   (begin (define a-val 1)))`,
		)},
		"test/circ-b.sld": &fstest.MapFile{Data: []byte(
			`(define-library (test circ-b)
			   (export b-val)
			   (import (scheme base) (test circ-a))
			   (begin (define b-val 2)))`,
		)},
	}

	// Malformed library file.
	malformedFS := fstest.MapFS{
		"test/bad.sld": &fstest.MapFile{Data: []byte(`not-a-library-form`)},
	}

	// Library name mismatch.
	mismatchFS := fstest.MapFS{
		"test/mismatch.sld": &fstest.MapFile{Data: []byte(
			`(define-library (test wrong-name)
			   (export x)
			   (import (scheme base))
			   (begin (define x 1)))`,
		)},
	}

	tcs := []struct {
		name string
		fs   fstest.MapFS
		code string
	}{
		{
			name: "missing library file",
			fs:   fstest.MapFS{},
			code: "(import (test nonexistent))",
		},
		{
			name: "circular dependency",
			fs:   circularFS,
			code: "(import (test circ-a))",
		},
		{
			name: "malformed library",
			fs:   malformedFS,
			code: "(import (test bad))",
		},
		{
			name: "library name mismatch",
			fs:   mismatchFS,
			code: "(import (test mismatch))",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := testutil.SetupEngineTest(t, tc.fs)
			// Use a panicking-tolerant eval since library errors may panic.
			func() {
				defer func() {
					r := recover()
					if r == nil {
						// If no panic, EvalSchemeInEnv should have called t.Fatal
						// on error. But some library errors are returned, not panicked.
					}
				}()
				// EvalSchemeInEnv calls qt.Assert on errors, which calls t.Fatal.
				// We need a version that returns errors. Use the raw pipeline.
				testutil.EvalSchemeInEnv(t, env, tc.code)
				t.Errorf("expected error for %s but got none", tc.name)
			}()
		})
	}
}
```

Note: The error test structure above is awkward because `EvalSchemeInEnv` fatals on
errors. You will likely need to add an `EvalSchemeInEnvMayFail` variant to
`machine/testutil/testutil.go` that returns `(values.Value, error)` instead of
fataling. Model it on the existing `EvalSchemeInEnv` but use error returns
instead of `qt.Assert`. This is a design choice for you to implement.

**Step 2: Run tests**

```bash
go test -v -run TestLibrarySystem ./machine/compilation/...
```

Expected: All PASS.

**Step 3: Check coverage improvement**

```bash
go test -coverprofile=/tmp/cov.out ./machine/compilation/...
go tool cover -func=/tmp/cov.out | grep 'machine/compilation/library_loader'
go tool cover -func=/tmp/cov.out | grep 'machine/compilation/library_bindings'
go tool cover -func=/tmp/cov.out | grep 'machine/compilation/compile_import'
```

Expected: Significant improvement in all three files.

**Step 4: Commit**

```
test(compilation): add library system Scheme-level tests

Exercises LoadLibrary, resolveImportSet, ApplyToExports,
CopyLibraryBindingsToEnv through real library import with
fstest.MapFS-backed virtual .sld files.
```

---

### Task 3: Library system white-box tests

**Files:**
- Modify: `machine/compilation/library_internal_test.go`

Cover error branches and internal functions unreachable from Scheme.

**Step 1: Write the tests**

Add to `library_internal_test.go`:

```go
// TestApplyToExports_Modifiers tests ImportSet modifier application directly.
func TestApplyToExports_Modifiers(t *testing.T) {
	lib := &CompiledLibrary{
		Name:    NewLibraryName("test", "lib"),
		Exports: map[string]string{"alpha": "alpha", "beta": "beta", "gamma": "gamma"},
	}

	tcs := []struct {
		name      string
		importSet *ImportSet
		wantKeys  []string
		wantErr   bool
	}{
		{
			name:      "no modifiers exports all",
			importSet: NewImportSet(lib.Name),
			wantKeys:  []string{"alpha", "beta", "gamma"},
		},
		{
			name: "only filter",
			importSet: &ImportSet{
				LibraryName: lib.Name,
				Only:        map[string]struct{}{"alpha": {}},
				Renames:     map[string]string{},
			},
			wantKeys: []string{"alpha"},
		},
		{
			name: "except filter",
			importSet: &ImportSet{
				LibraryName: lib.Name,
				Except:      map[string]struct{}{"gamma": {}},
				Renames:     map[string]string{},
			},
			wantKeys: []string{"alpha", "beta"},
		},
		{
			name: "prefix modifier",
			importSet: &ImportSet{
				LibraryName: lib.Name,
				Prefix:      "t:",
				Renames:     map[string]string{},
			},
			wantKeys: []string{"t:alpha", "t:beta", "t:gamma"},
		},
		{
			name: "rename modifier",
			importSet: &ImportSet{
				LibraryName: lib.Name,
				Renames:     map[string]string{"alpha": "a"},
			},
			wantKeys: []string{"a", "beta", "gamma"},
		},
		{
			name: "only nonexistent errors",
			importSet: &ImportSet{
				LibraryName: lib.Name,
				Only:        map[string]struct{}{"nonexistent": {}},
				Renames:     map[string]string{},
			},
			wantErr: true,
		},
		{
			name: "except nonexistent errors",
			importSet: &ImportSet{
				LibraryName: lib.Name,
				Except:      map[string]struct{}{"nonexistent": {}},
				Renames:     map[string]string{},
			},
			wantErr: true,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := tc.importSet.ApplyToExports(lib)
			if tc.wantErr {
				qt.Assert(t, err, qt.IsNotNil)
				return
			}
			qt.Assert(t, err, qt.IsNil)
			got := make([]string, 0, len(result))
			for k := range result {
				got = append(got, k)
			}
			sort.Strings(got)
			sort.Strings(tc.wantKeys)
			qt.Assert(t, got, qt.DeepEquals, tc.wantKeys)
		})
	}
}

// TestLibraryRegistryLoadingState tests IsLoading/StartLoading/FinishLoading.
func TestLibraryRegistryLoadingState(t *testing.T) {
	reg := NewLibraryRegistry()
	name := NewLibraryName("test", "lib")

	qt.Assert(t, reg.IsLoading(name), qt.IsFalse)

	reg.StartLoading(name)
	qt.Assert(t, reg.IsLoading(name), qt.IsTrue)

	reg.FinishLoading(name)
	qt.Assert(t, reg.IsLoading(name), qt.IsFalse)
}

// TestLoadLibrary_NilRegistry tests LoadLibrary with no library registry.
func TestLoadLibrary_NilRegistry(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	// env has no library registry set
	_, err := LoadLibrary(context.Background(), NewLibraryName("test"), env, machine.NewVMMacroEvaluator())
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no library registry")
}

// TestLoadLibrary_NoFileResolver tests LoadLibrary with no file resolver.
func TestLoadLibrary_NoFileResolver(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	reg := NewLibraryRegistry()
	env.SetLibraryRegistry(reg)
	// env has no file resolver set
	_, err := LoadLibrary(context.Background(), NewLibraryName("test"), env, machine.NewVMMacroEvaluator())
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no file resolver")
}
```

Add `"sort"` to imports if not present.

**Step 2: Run tests**

```bash
go test -v -run 'TestApplyToExports|TestLibraryRegistry|TestLoadLibrary' ./machine/compilation/...
```

Expected: All PASS.

**Step 3: Commit**

```
test(compilation): add library system white-box tests

Covers ApplyToExports modifiers, registry loading state,
LoadLibrary error branches.
```

---

### Task 4: Syntax-case Scheme-level tests

**Files:**
- Create: `machine/compilation/syntax_case_scheme_test.go`

Exercises `CompileSyntaxCase`, `compileSyntaxCaseClause`, `createPatternVarEnvironment`
through real Scheme code.

**Step 1: Write the test file**

```go
package compilation_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestSyntaxCase exercises syntax-case compilation through Scheme code.
//
// Source: compile_syntax_case.go (CompileSyntaxCase, compileSyntaxCaseClause,
// createPatternVarEnvironment).
func TestSyntaxCase(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "basic pattern match single clause",
			Code: `(define-syntax my-id
			  (lambda (stx)
			    (syntax-case stx ()
			      ((_ x) #'x))))
			(my-id 42)`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "multiple clauses first matches",
			Code: `(define-syntax my-val
			  (lambda (stx)
			    (syntax-case stx ()
			      ((_ a b) #'a)
			      ((_ a) #'a))))
			(my-val 10 20)`,
			Expected: values.NewInteger(10),
		},
		{
			Name: "multiple clauses second matches",
			Code: `(define-syntax my-val
			  (lambda (stx)
			    (syntax-case stx ()
			      ((_ a b) #'b)
			      ((_ a) #'a))))
			(my-val 99)`,
			Expected: values.NewInteger(99),
		},
		{
			Name: "syntax-case with fender",
			Code: `(define-syntax my-abs
			  (lambda (stx)
			    (syntax-case stx ()
			      ((_ x) (negative? (syntax->datum #'x))
			       #'(- 0 x))
			      ((_ x) #'x))))
			(my-abs -5)`,
			Expected: values.NewInteger(5),
		},
		{
			Name: "syntax-case with fender fallthrough",
			Code: `(define-syntax my-abs
			  (lambda (stx)
			    (syntax-case stx ()
			      ((_ x) (negative? (syntax->datum #'x))
			       #'(- 0 x))
			      ((_ x) #'x))))
			(my-abs 7)`,
			Expected: values.NewInteger(7),
		},
		{
			Name: "syntax-case with literals",
			Code: `(define-syntax my-match
			  (lambda (stx)
			    (syntax-case stx (foo)
			      ((_ foo) #'1)
			      ((_ x) #'2))))
			(my-match foo)`,
			Expected: values.NewInteger(1),
		},
		{
			Name: "syntax-case literals non-match falls through",
			Code: `(define-syntax my-match
			  (lambda (stx)
			    (syntax-case stx (foo)
			      ((_ foo) #'1)
			      ((_ x) #'2))))
			(my-match bar)`,
			Expected: values.NewInteger(2),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
```

Note: These tests use `define-syntax` with `lambda` + `syntax-case` which is the
R6RS procedural macro style. Some tests may need adjustment depending on how
Wile exposes `syntax-case`, `#'` (syntax template shorthand), and
`syntax->datum`. Verify the exact Scheme syntax Wile supports by checking
`compile_syntax_case.go` and the integration tests for prior examples. If `#'x`
is not supported, use `(syntax x)` instead. If `syntax->datum` is not available
for fender tests, simplify or skip the fender cases initially.

**Step 2: Run tests**

```bash
go test -v -run TestSyntaxCase ./machine/compilation/...
```

Iterate: if any tests fail due to syntax differences (e.g., `#'` vs `(syntax ...)`),
adjust the Scheme code to match Wile's actual syntax.

**Step 3: Check coverage**

```bash
go test -coverprofile=/tmp/cov.out ./machine/compilation/...
go tool cover -func=/tmp/cov.out | grep 'compile_syntax_case'
```

Expected: Significant improvement from 11.9%.

**Step 4: Commit**

```
test(compilation): add syntax-case Scheme-level tests

Exercises CompileSyntaxCase with single/multiple clauses,
fenders, and literals through real macro definitions.
```

---

### Task 5: With-syntax Scheme-level tests

**Files:**
- Create: `machine/compilation/with_syntax_scheme_test.go`

Exercises `CompileWithSyntax`, `compileWithSyntaxBody`, `buildWithSyntaxBegin`.

**Step 1: Write the test file**

```go
package compilation_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestWithSyntax exercises with-syntax compilation through Scheme code.
//
// Source: compile_with_syntax.go (CompileWithSyntax, compileWithSyntaxBody,
// buildWithSyntaxBegin).
func TestWithSyntax(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "with-syntax single binding",
			Code: `(define-syntax my-const
			  (lambda (stx)
			    (syntax-case stx ()
			      ((_ val)
			       (with-syntax ((result #'val))
			         #'result)))))
			(my-const 42)`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "with-syntax multiple bindings",
			Code: `(define-syntax swap-args
			  (lambda (stx)
			    (syntax-case stx ()
			      ((_ a b)
			       (with-syntax ((x #'b) (y #'a))
			         #'(list x y))))))
			(swap-args 1 2)`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(1)),
		},
		{
			Name: "with-syntax empty bindings body only",
			Code: `(define-syntax just-body
			  (lambda (stx)
			    (syntax-case stx ()
			      ((_)
			       (with-syntax ()
			         #'42)))))
			(just-body)`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "with-syntax multiple body expressions",
			Code: `(define-syntax multi-body
			  (lambda (stx)
			    (syntax-case stx ()
			      ((_ x)
			       (with-syntax ((v #'x))
			         #'(begin 1 2 v))))))
			(multi-body 99)`,
			Expected: values.NewInteger(99),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
```

Same caveat as Task 4: adjust `#'` syntax if Wile uses `(syntax ...)` instead.

**Step 2: Run tests**

```bash
go test -v -run TestWithSyntax ./machine/compilation/...
```

**Step 3: Check coverage**

```bash
go test -coverprofile=/tmp/cov.out ./machine/compilation/...
go tool cover -func=/tmp/cov.out | grep 'compile_with_syntax'
```

Expected: Improvement from 34.9%.

**Step 4: Commit**

```
test(compilation): add with-syntax Scheme-level tests

Exercises CompileWithSyntax with single/multiple bindings,
empty bindings, and multiple body expressions.
```

---

### Task 6: Expander and near-threshold coverage tests

**Files:**
- Create: `machine/compilation/expander_coverage_improvement_test.go`

Targeted Scheme-level tests for specific uncovered branches in the expander
and compiler files that are close to 80%.

**Step 1: Write the test file**

```go
package compilation_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestExpanderCoverageImprovement targets specific uncovered branches
// in expander and near-threshold compiler files.
func TestExpanderCoverageImprovement(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// expandWithBindingScope (0% in expander_primitive_forms.go)
		// Triggered by: (let ((=> #f)) (cond (#t => values)))
		{
			Name:     "with-binding-scope via let shadowing auxiliary",
			Code:     `(let ((=> #f)) (cond (#t 42)))`,
			Expected: values.NewInteger(42),
		},

		// case-lambda with rest args (extractIdentifierList in expander_lambda.go)
		{
			Name: "case-lambda with rest parameter",
			Code: `(define f (case-lambda
			  ((x) x)
			  ((x . rest) (apply + x rest))))
			(f 1 2 3)`,
			Expected: values.NewInteger(6),
		},

		// cond-expand with library requirement
		{
			Name:     "cond-expand library requirement scheme base",
			Code:     `(cond-expand ((library (scheme base)) 'yes) (else 'no))`,
			Expected: values.NewSymbol("yes"),
		},
		{
			Name:     "cond-expand library requirement missing",
			Code:     `(cond-expand ((library (nonexistent lib)) 'yes) (else 'no))`,
			Expected: values.NewSymbol("no"),
		},

		// cond-expand with and/or/not
		{
			Name:     "cond-expand and requirement",
			Code:     `(cond-expand ((and r7rs) 'yes) (else 'no))`,
			Expected: values.NewSymbol("yes"),
		},
		{
			Name:     "cond-expand or requirement",
			Code:     `(cond-expand ((or nonexistent r7rs) 'yes) (else 'no))`,
			Expected: values.NewSymbol("yes"),
		},
		{
			Name:     "cond-expand not requirement",
			Code:     `(cond-expand ((not nonexistent) 'yes) (else 'no))`,
			Expected: values.NewSymbol("yes"),
		},

		// syntax-rules with custom ellipsis
		{
			Name: "syntax-rules custom ellipsis",
			Code: `(define-syntax my-list
			  (syntax-rules ::: ()
			    ((_ x :::) (list x :::))))
			(my-list 1 2 3)`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},

		// eval-when expand phase
		{
			Name: "eval-when expand",
			Code: `(begin
			  (eval-when (expand)
			    (define-syntax ct-macro
			      (syntax-rules () ((_) 77))))
			  (ct-macro))`,
			Expected: values.NewInteger(77),
		},

		// define-for-syntax
		{
			Name: "define-for-syntax basic",
			Code: `(begin
			  (define-for-syntax helper-val 42)
			  (define-syntax use-helper
			    (syntax-rules ()
			      ((_) helper-val)))
			  (use-helper))`,
			Expected: values.NewInteger(42),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
```

Note: Some of these tests exercise advanced Scheme features. If any test uses
syntax Wile doesn't support yet (e.g., custom ellipsis, `eval-when`,
`define-for-syntax`), skip those specific cases. Check the relevant compiler
file to verify the feature exists before writing the test. The goal is to
exercise the code that IS there, not discover missing features.

**Step 2: Run tests, iterate on failures**

```bash
go test -v -run TestExpanderCoverageImprovement ./machine/compilation/...
```

Remove or adjust any tests that fail because the feature isn't supported.

**Step 3: Check total coverage**

```bash
go test -coverprofile=/tmp/cov.out ./machine/compilation/...
go tool cover -func=/tmp/cov.out | tail -1
```

Target: 80.0% or higher.

**Step 4: Commit**

```
test(compilation): add expander and near-threshold coverage tests

Targets specific uncovered branches: expandWithBindingScope,
extractIdentifierList, cond-expand library/and/or/not,
custom ellipsis, eval-when, define-for-syntax.
```

---

### Task 7: Measure, gap-fill, and remove exclusion

**Files:**
- Modify: `tools/sh/covercheck.sh` (remove exclusion)

**Step 1: Measure current coverage**

```bash
go test -coverprofile=/tmp/cov.out ./machine/compilation/...
go tool cover -func=/tmp/cov.out | grep 'machine/compilation/' | awk '{print $3, $1}' | sort -n | head -20
```

Identify any remaining files below 80% and write targeted tests as needed.
Repeat until the total reaches 80%.

**Step 2: Run full suite**

```bash
make lint && make test
```

**Step 3: Remove exclusion**

In `tools/sh/covercheck.sh`, remove `"machine/compilation"` from `EXCLUDED_PKGS`.

**Step 4: Verify covercheck passes**

```bash
make covercheck
```

Expected: All packages pass, including `machine/compilation`.

**Step 5: Commit**

```
chore(covercheck): remove machine/compilation from coverage exclusion

Coverage now at 80%+ after adding library system, syntax-case,
with-syntax, and expander tests.
```

---

## Summary

| Task | What | Files |
|------|------|-------|
| 1 | SetupEngineTest helper | `machine/testutil/testutil.go` |
| 2 | Library system Scheme-level tests | `machine/compilation/library_system_test.go` |
| 3 | Library system white-box tests | `machine/compilation/library_internal_test.go` |
| 4 | Syntax-case Scheme-level tests | `machine/compilation/syntax_case_scheme_test.go` |
| 5 | With-syntax Scheme-level tests | `machine/compilation/with_syntax_scheme_test.go` |
| 6 | Expander + near-threshold tests | `machine/compilation/expander_coverage_improvement_test.go` |
| 7 | Measure, gap-fill, remove exclusion | `tools/sh/covercheck.sh` |
