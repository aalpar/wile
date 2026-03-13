# Go Lint Extension — Phase 4 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build the `(wile goast lint)` extension that runs `go/analysis` passes on Go packages and returns diagnostics as s-expressions. Enables scripting over vet-style analyses: "what does nilness report for this package?" or "find all shadowed variables."

**Architecture:** New Go package `extensions/goastlint/` loads packages via `go/packages`, runs a selected set of `go/analysis/passes` analyzers through a minimal embedded driver, and maps `analysis.Diagnostic` to tagged-alist s-expressions. The `go-analyze-list` primitive is static (no loading). The `go-analyze` primitive requires the security gate. No opaque handles — diagnostics are pure data.

**Tech Stack:** `golang.org/x/tools/go/analysis`, `golang.org/x/tools/go/analysis/passes/*`, `golang.org/x/tools/go/packages` (all vendored via `golang.org/x/tools v0.42.0`)

**Design doc:** `plans/GO-STATIC-ANALYSIS.md` (Phase 4)

**Reference:** `extensions/goastcg/` — loading pattern. `extensions/goastssa/` — security gate and error sentinel pattern.

---

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Analysis driver | Custom minimal driver using `analysis.Pass` directly | `multichecker.Main` and `unitchecker` are CLI-oriented; embedding requires wiring `Pass` fields manually |
| Prerequisites | Topological sort over `a.Requires`; run all prerequisites before their dependents | Some analyzers need non-inspect prerequisites: `nilness` needs `buildssa` (which needs `ctrlflow`), `lostcancel` needs `ctrlflow`, `errorsas` needs `typeindexanalyzer`. Topo sort follows `Requires` pointers transitively — no need to import prerequisite packages directly. |
| Facts (inter-package) | No-op implementations (`AllObjectFacts`, `AllPackageFacts` return empty slices) | Removes a significant complexity tier; diagnostics alone cover the common scripting use case. **Consequence**: `ctrlflow` exports `noReturn` facts across packages; with no-op facts, `nilness` and `lostcancel` may under-report (false negatives) for cross-package no-return calls. Acceptable for scripting. |
| TypesSizes | Populated from `pkg.TypesSizes` via `NeedTypesSizes` load mode | Required by `shift` analyzer (`pass.TypesSizes.Sizeof`); nil would panic |
| Suggested fixes | Encoded as `#f` (not included) | Suggested fixes are AST patch operations; encoding them as s-expressions is deferred |
| Analyzer registry | Compile-time map of name → `*analysis.Analyzer` | No dynamic loading; analyzers from `go/analysis/passes` are all available at compile time |
| Curated set | 25 analyzers from `go/analysis/passes` | All prerequisite chains resolved by topoSort; excludes analyzers that require inter-package facts for correctness (e.g. `unusedresult`) |
| Variadic analyzers | `(go-analyze pattern "name1" "name2" ...)` | Natural for scripting: `(go-analyze "pkg" "nilness" "shadow")` |
| Unknown analyzer name | Return error, not silent skip | Fail loudly — typos should surface immediately |

### Curated Analyzer Set

These analyzers from `golang.org/x/tools/go/analysis/passes` are supported. Prerequisites are resolved automatically by the driver's topological sort:

| Name | Package | What it reports |
|---|---|---|
| `assign` | `passes/assign` | Useless assignments (x = x) |
| `bools` | `passes/bools` | Common mistakes with boolean operators |
| `composite` | `passes/composite` | Composite literal uses unkeyed fields |
| `copylocks` | `passes/copylock` | Locks passed or copied by value |
| `defers` | `passes/defers` | Common mistakes in defer statements |
| `directive` | `passes/directive` | Malformed //go: directives |
| `errorsas` | `passes/errorsas` | Second arg to errors.As is not a pointer |
| `httpresponse` | `passes/httpresponse` | Mistakes using net/http response body |
| `ifaceassert` | `passes/ifaceassert` | Impossible interface-to-interface type assertions |
| `loopclosure` | `passes/loopclosure` | Loop variable capture by closures |
| `lostcancel` | `passes/lostcancel` | Context cancellation function not called |
| `nilfunc` | `passes/nilfunc` | Useless comparisons between functions and nil |
| `nilness` | `passes/nilness` | Nil dereferences and tautological nil comparisons |
| `printf` | `passes/printf` | Consistency of Printf format strings |
| `shadow` | `passes/shadow` | Possibly unintended variable shadowing |
| `shift` | `passes/shift` | Shifts exceeding the width of integer type |
| `sortslice` | `passes/sortslice` | Calls to sort.Slice that do not use a slice |
| `stdmethods` | `passes/stdmethods` | Misspellings in signatures of error, String, etc. |
| `stringintconv` | `passes/stringintconv` | Conversions from integer types to strings |
| `structtag` | `passes/structtag` | Struct tags that do not follow the standard format |
| `testinggoroutine` | `passes/testinggoroutine` | t.Fatal called from a goroutine started by a test |
| `tests` | `passes/tests` | Common mistakes using testing.T |
| `timeformat` | `passes/timeformat` | Use of time.Format or time.Parse with invalid time format string |
| `unmarshal` | `passes/unmarshal` | Non-pointer values passed to unmarshal functions |
| `unreachable` | `passes/unreachable` | Unreachable code |

### S-expression Encoding

```scheme
;; go-analyze returns a list of diagnostic s-expressions
((diagnostic
   (analyzer . "nilness")
   (pos      . "server.go:42:5")
   (message  . "nil dereference of x")
   (category . ""))
 (diagnostic
   (analyzer . "shadow")
   (pos      . "handler.go:18:2")
   (message  . "declaration of err shadows declaration at handler.go:10:2")
   (category . ""))
 ...)

;; go-analyze-list returns a list of analyzer name strings (sorted)
("assign" "bools" "composite" "copylocks" ...)
```

### Package Structure

```
extensions/goastlint/
  doc.go                # Package documentation
  register.go           # Extension registration, LibraryNamer -> (wile goast lint)
  analyzers.go          # Curated analyzer registry (name -> *analysis.Analyzer)
  driver.go             # Minimal analysis.Pass driver
  prim_lint.go          # Primitive implementations
  prim_lint_test.go     # Primitive + integration tests (external package)
```

Note: no `mapper.go` — diagnostics are simple flat structs; mapping is inline in `prim_lint.go`.

---

### Task 1: Create goastlint extension skeleton

Create package structure, extension registration, and a stub `go-analyze` primitive.

**Files:**
- Create: `extensions/goastlint/doc.go`
- Create: `extensions/goastlint/register.go`
- Create: `extensions/goastlint/prim_lint.go`
- Create: `extensions/goastlint/prim_lint_test.go`

**Step 1: Write the failing test**

`extensions/goastlint/prim_lint_test.go` (external test package):

```go
package goastlint_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extgoastlint "github.com/aalpar/wile/extensions/goastlint"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgoastlint.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

func runScheme(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

func runSchemeExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNotNil)
}

func TestExtensionLibraryName(t *testing.T) {
	type libraryNamer interface {
		LibraryName() []string
	}
	namer, ok := extgoastlint.Extension.(libraryNamer)
	qt.New(t).Assert(ok, qt.IsTrue)
	qt.New(t).Assert(namer.LibraryName(), qt.DeepEquals, []string{"wile", "goast", "lint"})
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastlint/... 2>&1 | head -20`
Expected: FAIL — package doesn't exist.

**Step 3: Write skeleton files**

`extensions/goastlint/doc.go`:
```go
// Package goastlint exposes Go's go/analysis framework as Scheme
// s-expressions. Run named static analysis passes on Go packages
// and query their diagnostics with standard Scheme list operations.
package goastlint
```

`extensions/goastlint/register.go`:
```go
package goastlint

import "github.com/aalpar/wile/registry"

// lintExtension wraps Extension to implement LibraryNamer.
type lintExtension struct {
	registry.Extension
}

// LibraryName returns (wile goast lint) for R7RS import.
func (p *lintExtension) LibraryName() []string {
	return []string{"wile", "goast", "lint"}
}

// Extension is the lint extension entry point.
var Extension registry.Extension = &lintExtension{
	Extension: registry.NewExtension("goast-lint", AddToRegistry),
}

// Builder aggregates all lint registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all lint primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name: "go-analyze", ParamCount: 2, IsVariadic: true,
			Impl:       PrimGoAnalyze,
			Doc:        "Runs named go/analysis passes on a Go package and returns diagnostics.",
			ParamNames: []string{"pattern", "analyzer-names"},
			Category:   "goast-lint",
		},
		{
			Name: "go-analyze-list", ParamCount: 0,
			Impl:       PrimGoAnalyzeList,
			Doc:        "Returns a sorted list of available analyzer names.",
			ParamNames: []string{},
			Category:   "goast-lint",
		},
	}, registry.PhaseRuntime)
	return nil
}
```

`extensions/goastlint/prim_lint.go` (stub):
```go
package goastlint

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

var (
	errAnalyzeBuildError   = werr.NewStaticError("analyze build error")
	errAnalyzeUnknownName  = werr.NewStaticError("unknown analyzer name")
)

// PrimGoAnalyze stub: validates first arg is string, returns empty list.
func PrimGoAnalyze(mc *machine.MachineContext) error {
	// Stub — filled in by Task 3.
	mc.SetValue(values.EmptyList)
	return nil
}

// PrimGoAnalyzeList stub: returns empty list.
func PrimGoAnalyzeList(mc *machine.MachineContext) error {
	// Filled in by Task 2.
	mc.SetValue(values.EmptyList)
	return nil
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v ./extensions/goastlint/... -run TestExtensionLibraryName`
Expected: PASS.

**Step 5: Run lint**

Run: `make lint`
Expected: Clean.

**Step 6: Commit**

```
feat(goastlint): add (wile goast lint) extension skeleton

Creates the package structure with LibraryNamer support and
stub primitives for go-analyze and go-analyze-list.
```

---

### Task 2: Analyzer registry and go-analyze-list

Build the compile-time registry of analyzers, implement `go-analyze-list`.

**Files:**
- Create: `extensions/goastlint/analyzers.go`
- Modify: `extensions/goastlint/prim_lint.go`
- Modify: `extensions/goastlint/prim_lint_test.go`

**Step 1: Write the failing test**

Append to `prim_lint_test.go`:

```go
func TestGoAnalyzeList_ReturnsStrings(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine, `(pair? (go-analyze-list))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoAnalyzeList_ContainsKnownAnalyzers(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	for _, name := range []string{"nilness", "shadow", "assign", "unreachable"} {
		result := runScheme(t, engine, `
			(let loop ((names (go-analyze-list)))
				(cond
					((null? names) #f)
					((equal? (car names) "`+name+`") #t)
					(else (loop (cdr names)))))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue,
			qt.Commentf("expected %q in go-analyze-list", name))
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastlint/... -run TestGoAnalyzeList`
Expected: FAIL — returns empty list.

**Step 3: Implement the analyzer registry**

`extensions/goastlint/analyzers.go`:

```go
package goastlint

import (
	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/analysis/passes/assign"
	"golang.org/x/tools/go/analysis/passes/bools"
	"golang.org/x/tools/go/analysis/passes/composite"
	"golang.org/x/tools/go/analysis/passes/copylock"
	"golang.org/x/tools/go/analysis/passes/defers"
	"golang.org/x/tools/go/analysis/passes/directive"
	"golang.org/x/tools/go/analysis/passes/errorsas"
	"golang.org/x/tools/go/analysis/passes/httpresponse"
	"golang.org/x/tools/go/analysis/passes/ifaceassert"
	"golang.org/x/tools/go/analysis/passes/loopclosure"
	"golang.org/x/tools/go/analysis/passes/lostcancel"
	"golang.org/x/tools/go/analysis/passes/nilfunc"
	"golang.org/x/tools/go/analysis/passes/nilness"
	"golang.org/x/tools/go/analysis/passes/printf"
	"golang.org/x/tools/go/analysis/passes/shadow"
	"golang.org/x/tools/go/analysis/passes/shift"
	"golang.org/x/tools/go/analysis/passes/sortslice"
	"golang.org/x/tools/go/analysis/passes/stdmethods"
	"golang.org/x/tools/go/analysis/passes/stringintconv"
	"golang.org/x/tools/go/analysis/passes/structtag"
	"golang.org/x/tools/go/analysis/passes/testinggoroutine"
	"golang.org/x/tools/go/analysis/passes/tests"
	"golang.org/x/tools/go/analysis/passes/timeformat"
	"golang.org/x/tools/go/analysis/passes/unmarshal"
	"golang.org/x/tools/go/analysis/passes/unreachable"
)

// analyzerRegistry maps analyzer names to their *analysis.Analyzer.
// Prerequisites vary: most use inspect; nilness needs buildssa→ctrlflow,
// lostcancel needs ctrlflow, errorsas needs typeindexanalyzer.
// The driver's topological sort resolves all prerequisite chains automatically.
var analyzerRegistry = map[string]*analysis.Analyzer{
	"assign":          assign.Analyzer,
	"bools":           bools.Analyzer,
	"composite":       composite.Analyzer,
	"copylocks":       copylock.Analyzer,
	"defers":          defers.Analyzer,
	"directive":       directive.Analyzer,
	"errorsas":        errorsas.Analyzer,
	"httpresponse":    httpresponse.Analyzer,
	"ifaceassert":     ifaceassert.Analyzer,
	"loopclosure":     loopclosure.Analyzer,
	"lostcancel":      lostcancel.Analyzer,
	"nilfunc":         nilfunc.Analyzer,
	"nilness":         nilness.Analyzer,
	"printf":          printf.Analyzer,
	"shadow":          shadow.Analyzer,
	"shift":           shift.Analyzer,
	"sortslice":       sortslice.Analyzer,
	"stdmethods":      stdmethods.Analyzer,
	"stringintconv":   stringintconv.Analyzer,
	"structtag":       structtag.Analyzer,
	"testinggoroutine": testinggoroutine.Analyzer,
	"tests":           tests.Analyzer,
	"timeformat":      timeformat.Analyzer,
	"unmarshal":       unmarshal.Analyzer,
	"unreachable":     unreachable.Analyzer,
}
```

**Step 4: Implement go-analyze-list**

Replace the stub `PrimGoAnalyzeList` in `prim_lint.go`:

```go
func PrimGoAnalyzeList(mc *machine.MachineContext) error {
	names := make([]string, 0, len(analyzerRegistry))
	for name := range analyzerRegistry {
		names = append(names, name)
	}
	sort.Strings(names)
	result := make([]values.Value, len(names))
	for i, name := range names {
		result[i] = goast.Str(name)
	}
	mc.SetValue(goast.ValueList(result))
	return nil
}
```

Add `"sort"` to the import block in `prim_lint.go`.

**Step 5: Run test to verify it passes**

Run: `go test -v ./extensions/goastlint/... -run TestGoAnalyzeList`
Expected: PASS.

**Step 6: Run lint**

Run: `make lint`
Expected: Clean.

**Step 7: Commit**

```
feat(goastlint): add analyzer registry and go-analyze-list

Compile-time map of 25 analyzers from go/analysis/passes. All use
inspect as their only prerequisite. go-analyze-list returns a sorted
list of available analyzer names.
```

---

### Task 3: Minimal analysis driver

The core infrastructure: an embedded `analysis.Pass` driver that runs the `inspect` analyzer first and feeds its result to dependent analyzers.

**Files:**
- Create: `extensions/goastlint/driver.go`

**Step 1: No test for driver directly** — driver is tested through `go-analyze` in Task 4. Implement directly.

**Step 2: Implement the driver**

`extensions/goastlint/driver.go`:

```go
package goastlint

import (
	"go/token"
	"go/types"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/packages"
)

// diagnostic holds a captured analysis diagnostic.
type diagnostic struct {
	analyzerName string
	diag         analysis.Diagnostic
}

// runAnalyzers runs the requested analyzers on a loaded package, first
// resolving and running all prerequisites in topological order.
// Only diagnostics from the originally requested analyzers are returned;
// prerequisite results are available to dependents via Pass.ResultOf but
// their diagnostics are silently discarded.
func runAnalyzers(pkg *packages.Package, fset *token.FileSet, analyzers []*analysis.Analyzer) []diagnostic {
	// Collect all analyzers (including prerequisites) in topological order.
	ordered := topoSort(analyzers)

	// Track which analyzers were explicitly requested (not just prerequisites).
	requested := make(map[*analysis.Analyzer]bool, len(analyzers))
	for _, a := range analyzers {
		requested[a] = true
	}

	resultOf := make(map[*analysis.Analyzer]interface{})
	failed := make(map[*analysis.Analyzer]bool)
	var diags []diagnostic

	for _, a := range ordered {
		// Skip if any prerequisite failed — avoids nil-deref in ResultOf lookups.
		skip := false
		for _, req := range a.Requires {
			if failed[req] {
				skip = true
				break
			}
		}
		if skip {
			failed[a] = true
			continue
		}

		var collected []analysis.Diagnostic
		pass := makePass(pkg, fset, a, resultOf, func(d analysis.Diagnostic) {
			if requested[a] {
				collected = append(collected, d)
			}
		})
		result, err := a.Run(pass)
		if err != nil {
			failed[a] = true
			continue
		}
		if result != nil {
			resultOf[a] = result
		}
		for _, d := range collected {
			diags = append(diags, diagnostic{
				analyzerName: a.Name,
				diag:         d,
			})
		}
	}
	return diags
}

// topoSort returns all analyzers (including transitive prerequisites) in
// topological order: prerequisites before their dependents.
func topoSort(analyzers []*analysis.Analyzer) []*analysis.Analyzer {
	var ordered []*analysis.Analyzer
	seen := make(map[*analysis.Analyzer]bool)
	var visit func(a *analysis.Analyzer)
	visit = func(a *analysis.Analyzer) {
		if seen[a] {
			return
		}
		seen[a] = true
		for _, req := range a.Requires {
			visit(req)
		}
		ordered = append(ordered, a)
	}
	for _, a := range analyzers {
		visit(a)
	}
	return ordered
}

// makePass constructs an analysis.Pass for the given analyzer and package.
// Fact functions are no-ops: single-package analysis, no cross-package facts.
// This means ctrlflow's noReturn facts won't propagate across packages,
// so nilness and lostcancel may under-report (false negatives). Acceptable.
func makePass(
	pkg *packages.Package,
	fset *token.FileSet,
	a *analysis.Analyzer,
	resultOf map[*analysis.Analyzer]interface{},
	report func(analysis.Diagnostic),
) *analysis.Pass {
	return &analysis.Pass{
		Analyzer:   a,
		Fset:       fset,
		Files:      pkg.Syntax,
		Pkg:        pkg.Types,
		TypesInfo:  pkg.TypesInfo,
		TypesSizes: pkg.TypesSizes,
		ResultOf:   resultOf,
		Report:     report,
		AllObjectFacts:    func() []analysis.ObjectFact { return nil },
		AllPackageFacts:   func() []analysis.PackageFact { return nil },
		ExportObjectFact:  func(types.Object, analysis.Fact) {},
		ExportPackageFact: func(analysis.Fact) {},
		ImportObjectFact:  func(types.Object, analysis.Fact) bool { return false },
		ImportPackageFact: func(*types.Package, analysis.Fact) bool { return false },
	}
}
```

**Step 3: Run lint**

Run: `make lint`
Expected: Clean.

**Step 4: Commit**

```
feat(goastlint): add minimal analysis driver with topological prerequisite resolution

topoSort collects transitive prerequisites and runs them in dependency
order. makePass wires up analysis.Pass with no-op fact functions
(single-package analysis; no cross-package fact persistence). Only
diagnostics from explicitly requested analyzers are reported.
```

---

### Task 4: Implement go-analyze

The main primitive: load packages, run analyzers, map diagnostics to s-expressions.

**Files:**
- Modify: `extensions/goastlint/prim_lint.go`
- Modify: `extensions/goastlint/prim_lint_test.go`

**Step 1: Write the failing test**

Append to `prim_lint_test.go`:

```go
func TestGoAnalyze_ReturnsListForKnownPackage(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Run a simple analyzer on a known package.
	// Result may be empty (no issues) or non-empty — both are valid.
	result := runScheme(t, engine,
		`(list? (go-analyze "github.com/aalpar/wile/extensions/goast" "assign"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoAnalyze_DiagnosticStructure(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// If any diagnostics are returned, verify they have expected fields.
	result := runScheme(t, engine, `
		(let ((diags (go-analyze "github.com/aalpar/wile/extensions/goast" "assign")))
			(if (null? diags) #t
				(let ((d (car diags)))
					(and (eq? (car d) 'diagnostic)
					     (string? (cdr (assoc 'analyzer (cdr d))))
					     (string? (cdr (assoc 'pos      (cdr d))))
					     (string? (cdr (assoc 'message  (cdr d))))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoAnalyze_MultipleAnalyzers(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine,
		`(list? (go-analyze "github.com/aalpar/wile/extensions/goast" "assign" "unreachable"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoAnalyze_Errors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{name: "wrong pattern type", code: `(go-analyze 42 "assign")`},
		{name: "unknown analyzer name", code: `(go-analyze "github.com/aalpar/wile/extensions/goast" "no-such-analyzer")`},
		{name: "nonexistent package", code: `(go-analyze "github.com/aalpar/wile/does-not-exist-xyz" "assign")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeExpectError(t, engine, tc.code)
		})
	}
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v ./extensions/goastlint/... -run TestGoAnalyze`
Expected: FAIL — stub returns empty list and no error for unknown analyzer.

**Step 3: Implement go-analyze**

Replace the stub `PrimGoAnalyze` in `prim_lint.go`:

```go
func PrimGoAnalyze(mc *machine.MachineContext) error {
	pattern, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-analyze")
	if err != nil {
		return err
	}

	// Collect and validate analyzer names from variadic args.
	var analyzers []*analysis.Analyzer
	rest := mc.Arg(1)
	tuple, ok := rest.(values.Tuple)
	if ok {
		for !values.IsEmptyList(tuple) {
			pair, ok := tuple.(*values.Pair)
			if !ok {
				break
			}
			nameVal, ok := pair.Car().(*values.String)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotAString,
					"go-analyze: analyzer names must be strings")
			}
			a, found := analyzerRegistry[nameVal.Value]
			if !found {
				return werr.WrapForeignErrorf(errAnalyzeUnknownName,
					"go-analyze: unknown analyzer %q; use go-analyze-list for available names",
					nameVal.Value)
			}
			analyzers = append(analyzers, a)
			tuple, ok = pair.Cdr().(values.Tuple)
			if !ok {
				break
			}
		}
	}

	if len(analyzers) == 0 {
		mc.SetValue(values.EmptyList)
		return nil
	}

	err = security.Check(mc.Context(), security.AccessRequest{
		Resource: security.ResourceProcess,
		Action:   security.ActionLoad,
		Target:   "go",
	})
	if err != nil {
		return err
	}

	fset := token.NewFileSet()
	cfg := &packages.Config{
		Mode: packages.NeedName |
			packages.NeedFiles |
			packages.NeedSyntax |
			packages.NeedTypes |
			packages.NeedTypesInfo |
			packages.NeedTypesSizes |
			packages.NeedImports |
			packages.NeedDeps,
		Context: mc.Context(),
		Fset:    fset,
	}

	pkgs, loadErr := packages.Load(cfg, pattern.Value)
	if loadErr != nil {
		return werr.WrapForeignErrorf(errAnalyzeBuildError,
			"go-analyze: %s: %s", pattern.Value, loadErr)
	}

	var errs []string
	for _, pkg := range pkgs {
		for _, e := range pkg.Errors {
			errs = append(errs, e.Error())
		}
	}
	if len(errs) > 0 {
		return werr.WrapForeignErrorf(errAnalyzeBuildError,
			"go-analyze: %s: %s", pattern.Value, strings.Join(errs, "; "))
	}

	// Run analyzers on each loaded package; collect all diagnostics.
	var allDiags []diagnostic
	for _, pkg := range pkgs {
		allDiags = append(allDiags, runAnalyzers(pkg, fset, analyzers)...)
	}

	// Map diagnostics to s-expressions.
	result := make([]values.Value, len(allDiags))
	for i, d := range allDiags {
		pos := fset.Position(d.diag.Pos)
		fields := []values.Value{
			goast.Field("analyzer", goast.Str(d.analyzerName)),
			goast.Field("pos", goast.Str(pos.String())),
			goast.Field("message", goast.Str(d.diag.Message)),
			goast.Field("category", goast.Str(d.diag.Category)),
		}
		result[i] = goast.Node("diagnostic", fields...)
	}
	mc.SetValue(goast.ValueList(result))
	return nil
}
```

Add required imports to `prim_lint.go`:

```go
import (
	"go/token"
	"sort"
	"strings"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/packages"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)
```

**Step 4: Run tests**

Run: `go test -v ./extensions/goastlint/... -timeout 120s`
Expected: PASS.

**Step 5: Run lint**

Run: `make lint`
Expected: Clean.

**Step 6: Commit**

```
feat(goastlint): implement go-analyze

Loads packages via go/packages, validates analyzer names against
the registry, runs the analysis driver, and maps diagnostics to
(diagnostic (analyzer . "...") (pos . "...") (message . "..."))
s-expressions. Unknown analyzer names return an immediate error.
```

---

### Task 5: Integration test and plan update

End-to-end test on a real package. Validates that `go-analyze` produces correctly structured diagnostics and that analyzers can be combined.

**Files:**
- Modify: `extensions/goastlint/prim_lint_test.go`

**Step 1: Write the integration test**

Append to `prim_lint_test.go`:

```go
func TestIntegration_AnalyzeRealPackage(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Run multiple analyzers at once on the goastlint package itself.
	// Every diagnostic (if any) must have correct structure.
	result := runScheme(t, engine, `
		(define diags
			(go-analyze "github.com/aalpar/wile/extensions/goastlint"
			            "assign" "unreachable" "structtag"))

		(define (nf node key)
			(let ((e (assoc key (cdr node))))
				(if e (cdr e) #f)))

		;; All diagnostics must be properly tagged.
		(let loop ((ds diags))
			(if (null? ds) #t
				(let ((d (car ds)))
					(and (eq? (car d) 'diagnostic)
					     (string? (nf d 'analyzer))
					     (string? (nf d 'pos))
					     (string? (nf d 'message))
					     (loop (cdr ds))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestIntegration_AllAnalyzersRunnable(t *testing.T) {
	// Verify that every registered analyzer can run without panicking.
	// Run go-analyze-list and run each analyzer on a simple, known package.
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine, `
		(define names (go-analyze-list))

		;; Run each analyzer; any error is a test failure.
		;; We only verify it returns a list (no crash).
		(let loop ((ns names) (ok #t))
			(if (null? ns) ok
				(let ((diags (go-analyze
				              "github.com/aalpar/wile/extensions/goast"
				              (car ns))))
					(loop (cdr ns) (and ok (list? diags))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}
```

**Step 2: Run integration tests**

Run: `go test -v ./extensions/goastlint/... -run TestIntegration -timeout 180s`
Expected: PASS.

**Step 3: Run full test suite**

Run: `make lint && make test`
Expected: All packages pass.

**Step 4: Run covercheck**

Run: `make covercheck`
Expected: `extensions/goastlint` >= 80%.

**Step 5: Update plan status**

In `plans/GO-STATIC-ANALYSIS.md`, update the Phase 4 heading:

```
### Phase 4: `(wile goast lint)` — Analysis Passes  ✓ Complete
```

Update `plans/CLAUDE.md` to add the plan file entry:

```
| `GO-LINT-PHASE-4.md` | Analysis passes extension implementation plan (Phase 4 of GO-STATIC-ANALYSIS) | Complete |
```

**Step 6: Commit**

```
docs: mark GO-STATIC-ANALYSIS Phase 4 complete
```

---

## Post-implementation checklist

- [ ] All 5 task commits on branch
- [ ] `make lint` clean
- [ ] `make test` passes (full suite)
- [ ] `make covercheck` passes
- [ ] `plans/GO-STATIC-ANALYSIS.md` updated: Phase 4 → "Complete"
- [ ] `plans/CLAUDE.md` updated with plan file entry
- [ ] All 25 analyzers in registry are importable (no missing vendored packages)
- [ ] Unknown analyzer name returns error, not silent skip
- [ ] Empty analyzer list (no names given) returns empty list without error
- [ ] `TestIntegration_AllAnalyzersRunnable` passes — every registered analyzer runs without crash

## Primitives summary

| Primitive | Signature | Security | Description |
|---|---|---|---|
| `go-analyze` | `(go-analyze pattern analyzer-name ...)` | `ResourceProcess`/`ActionLoad` | Run named analyzers on a package; return list of `diagnostic` s-expressions |
| `go-analyze-list` | `(go-analyze-list)` | None | Return sorted list of available analyzer name strings |

## Design notes for future work

**Analyzer facts**: The current driver no-ops all fact import/export functions. Adding cross-package fact support would require: loading the full import graph, running analyzers in topological order over all packages, and persisting facts between runs. This is the complexity tier that `unitchecker` exists to handle. Add only if a concrete use case requires it.

**Suggested fixes**: `analysis.Diagnostic.SuggestedFixes` carries AST patch operations. These could be encoded as `(suggested-fix (message . "...") (edits . ((edit (pos . "...") (end . "...") (new-text . "...")) ...)))`. Deferred until a use case requires applying fixes from Scheme.

**Additional analyzers**: The driver already handles arbitrary prerequisite depths via topological sort — `nilness` → `buildssa` → `ctrlflow` → `inspect` is a 4-deep chain that works today. Third-party analyzers (e.g., `SA*` from staticcheck) can be added if they don't require cross-package facts beyond what the no-op stubs provide.
