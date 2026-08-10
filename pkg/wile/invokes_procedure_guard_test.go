// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Static structural guard for the PrimitiveSpec.InvokesProcedure annotation
// (compiler-review remediation Task 5A).
//
// PrimitiveSpec.InvokesProcedure defaults false, and false = "capture-safe":
// registry/apply.go stamps BindingMeta.CaptureSafe = !InvokesProcedure, which the
// frame-reclaim classifier trusts to release an env frame across a tail call. The
// flipped default is therefore a SOUNDNESS COMMITMENT — a procedure-invoking
// primitive that forgets the annotation is silently trusted as capture-safe, and
// the corruption (a frame released across a capturing call) only surfaces under
// continuation re-entry, which ordinary unit tests do not exercise.
//
// TestInvokesProcedureCompleteness (capture_safety_test.go) pins a hand-curated
// list behaviorally but, by its own admission, "cannot catch a brand-new
// procedure-invoker absent from this list." This file closes that gap with a
// STATIC check: it discovers, by AST analysis, which registered primitives
// transitively reach a procedure-invocation sink, and asserts every one of them
// carries InvokesProcedure:true. Adding a procedure-invoking primitive without the
// annotation fails CI here, before it can miscompile.
//
// Why AST and not ruleguard or x/tools SSA:
//   - ruleguard matches syntax within a single function; it cannot follow the
//     Impl-field-to-function-body link, nor reason transitively or via dataflow.
//   - golang.org/x/tools (SSA + callgraph) would be the textbook tool, but the
//     module's go.mod is deliberately minimal (CLAUDE.md: "prefer standard library
//     over new dependencies"), and that dependency tree is large. The analysis
//     needed here is small enough for the stdlib go/ast + go/parser.
//
// Soundness model. The check asserts annotated ⊇ discovered (every discovered
// invoker is annotated). The discovery deliberately OVER-approximates the
// "invokes" predicate (recursing into nested closures, resolving call edges by
// name): a false positive in discovery merely demands an annotation on a safe
// primitive (a loud, fixable CI failure), whereas a false negative would be the
// silent corruption we are guarding against. The reverse direction
// (annotated && !discovered, e.g. `raise`, which signals via a returned
// ErrExceptionEscape that the VM dispatches outside PrimRaise's static call graph)
// is harmless over-annotation and is NOT asserted.
//
// Scope, and its known hole. Discovery analyzes exactly the packages that host
// registered primitive Impls (derived from the live registry), and resolves call
// edges within each such package. A sink reached only through a helper in a
// non-primitive package is therefore MISSED unless that helper is itself named in
// procInvokingDriverSelectors.
//
// This paragraph used to claim "no such case exists today". It was false, and it
// was false about the most consequential primitive in the tree: `error` reached
// ApplyCallable through machine.RaiseInPlace, a helper in pkg/machine, and went
// unannotated for the whole life of this guard (review-wave-1 item 5).
//
// It was false twice. `expand` and `expand-once` reach their sink through
// pkg/machine/compilation (ExpanderTimeContinuation.ExpandExpression /
// ExpandOnce) and then pkg/machine (invokeTransformerClosure); no registered
// primitive Impl lives in either package, so the analyzer parses neither and
// discovered neither. `expand` carried a hand-placed annotation; `expand-once`
// carried none, which is what the selectors below now catch (review-wave-4
// item 10).
//
// Both instances are closed; the structural hole is not. Every sink reachable
// only under a name not in the two selector sets above is still invisible, and
// nothing here detects that.
// TestInvokesProcedureCompleteness remains the behavioral backstop, and
// TestProcedureInvokersMatchesInvokesProcedure (capture_safety_test.go) now
// keeps its list derived from the annotations rather than curated.

package wile

import (
	"context"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// modulePath is this module's Go import-path root. The fully-qualified function
// names runtime.FuncForPC reports (via resolveImpl) are <modulePath>/<dir>.<Func>,
// which is the join key between the registry side (resolveImpl) and the AST side
// (importPath + "." + FuncDecl.Name). TestInvokesProcedureGuardModulePath asserts
// this matches go.mod so a module rename cannot silently make the join vacuous.
const modulePath = "github.com/aalpar/wile"

// Procedure-invocation sinks — the two ways a Go primitive runs Scheme code:
//
//   - applyCallableSelector: a call whose selector is ApplyCallable. The name is
//     unique to *machine.MachineContext, so matching by selector name alone is
//     sound (no other type in the tree defines an ApplyCallable method).
//   - runSelector on a sub-context: a no-arg .Run() whose receiver is a local
//     assigned from one of the NewSubContext* factories. The sub-context qualifier
//     is load-bearing — extensions/process calls exec.Cmd.Run(), which is NOT a
//     Scheme sink. We separate the two by intra-function dataflow (which locals are
//     sub-contexts) instead of a type checker, keeping the guard stdlib-only.
const (
	applyCallableSelector = "ApplyCallable"
	runSelector           = "Run"
)

// subContextFactories are the *MachineContext methods that mint a sub-context. A
// local assigned from one of these is a sub-context whose .Run() executes Scheme.
var subContextFactories = map[string]bool{
	"NewSubContext":             true,
	"NewSubContextWithWinding":  true,
	"NewSubContextWithTemplate": true,
}

// procInvokingDriverSelectors are machine-package drivers that ALWAYS run a Scheme
// procedure on the live chain, so a primitive that calls one is a procedure-invoker.
// They are sinks regardless of receiver — like ApplyCallable, and unlike Run (a sink
// only on a sub-context; exec.Cmd.Run is not Scheme). RunBodyUnder* push a
// continuation frame and inline-apply a body (the call-with-values / call-with-exit /
// call-with-continuation-prompt reification); RunWithinBoundary drives a sub-context
// that may run one. Without these, the reified boundary primitives (whose ApplyCallable
// now lives behind these machine helpers, in a package the analyzer does not parse)
// would no longer be discovered as invokers — a false-negative that the InvokesProcedure
// soundness contract treats as corruption.
//
// Not all of them are *MachineContext METHODS. machine.RaiseInPlace
// (machine/exception_raise.go) is a package-level function taking the context as
// its first argument, and it runs the installed handler INLINE on the live chain
// — the same sink shape, reached through a different spelling. It is here because
// matching by method-ness would have kept excluding it, and matching by selector
// name (see the sinkCall matcher) does not care.
//
// This entry is the review-wave-1 item 5 fix. `error` reached RaiseInPlace and
// carried no InvokesProcedure stamp for as long as this guard has existed,
// because RaiseInPlace was not a listed sink — the guard was blind to the exact
// primitive it most needed to catch. Adding the selector closes the INSTANCE. It
// does not close the selector-keying that makes the guard blind in kind: a sink
// reached under any name not listed here is still invisible.
//
// ExpandExpression/ExpandOnce are the second instance, and they are not even
// machine-package names: they are *ExpanderTimeContinuation methods in
// pkg/machine/compilation, and their sink (invokeTransformerClosure, an
// Apply+Run on a sub-context) is a further package away in pkg/machine. Neither
// package hosts a registered primitive Impl, so the analyzer parses neither, and
// expand/expand-once were invisible to it — `expand` was annotated by hand and
// `expand-once` was not annotated at all. Selector matching does not care which
// package a name comes from, so listing them here reaches both.
var procInvokingDriverSelectors = map[string]bool{
	"RunBodyUnderFrame":     true,
	"RunBodyUnderConsumer":  true,
	"RunBodyUnderExitFrame": true,
	"RunBodyUnderPrompt":    true,
	"RunWithinBoundary":     true,
	"RaiseInPlace":          true,
	"ExpandExpression":      true,
	"ExpandOnce":            true,
}

// closureSuffix matches the trailing .func1[.func2...] that runtime function names
// carry for closures. A primitive registered via a closure Impl (e.g. sin →
// makeComplexPrimitive.func10) joins to its enclosing top-level declaration, which
// is the granularity the AST analyzer keys on.
var closureSuffix = regexp.MustCompile(`(\.func\d+)+$`)

// procInvokeAnalyzer accumulates package ASTs and computes, by intra-package
// transitive closure over static call edges, the set of top-level functions that
// reach a procedure-invocation sink. Keys are fully-qualified <importpath>.<Func>.
type procInvokeAnalyzer struct {
	direct map[string]bool     // function directly contains a sink (incl. nested closures)
	edges  map[string][]string // function -> resolved same-package callee keys
}

func newProcInvokeAnalyzer() *procInvokeAnalyzer {
	return &procInvokeAnalyzer{
		direct: map[string]bool{},
		edges:  map[string][]string{},
	}
}

// addPackage analyzes one package's already-parsed files. importPath forms the
// prefix of every key the package contributes. Call edges are resolved only to
// package-local declarations: cross-package callees cannot reach a sink today
// because every sink is co-located with its primitive (see the file header), and
// limiting resolution to local names keeps the over-approximation tight.
func (a *procInvokeAnalyzer) addPackage(importPath string, files []*ast.File) {
	declNames := map[string]bool{}
	for _, f := range files {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if !ok {
				continue
			}
			declNames[fd.Name.Name] = true
		}
	}
	for _, f := range files {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if !ok || fd.Body == nil {
				continue
			}
			key := importPath + "." + fd.Name.Name
			subVars := collectSubContextVars(fd.Body)
			if funcHasSink(fd.Body, subVars) {
				a.direct[key] = true
			}
			for _, callee := range collectIntraPackageCallees(fd.Body, declNames) {
				a.edges[key] = append(a.edges[key], importPath+"."+callee)
			}
		}
	}
}

// computeInvokes returns the transitive closure: a function invokes a procedure if
// it directly contains a sink or statically calls a package-local function that
// does. Iterated to a fixed point.
func (a *procInvokeAnalyzer) computeInvokes() map[string]bool {
	invokes := make(map[string]bool, len(a.direct))
	for k, v := range a.direct {
		if v {
			invokes[k] = true
		}
	}
	for {
		changed := false
		for k, callees := range a.edges {
			if invokes[k] {
				continue
			}
			for _, c := range callees {
				if invokes[c] {
					invokes[k] = true
					changed = true
					break
				}
			}
		}
		if !changed {
			break
		}
	}
	return invokes
}

// collectSubContextVars returns the set of local identifier names assigned from a
// NewSubContext* factory anywhere in body (including nested closures — the
// over-approximation is safe). These are the receivers whose .Run() is a Scheme
// sink, as opposed to exec.Cmd.Run() and other unrelated Run methods.
func collectSubContextVars(body *ast.BlockStmt) map[string]bool {
	vars := map[string]bool{}
	ast.Inspect(body, func(n ast.Node) bool {
		as, ok := n.(*ast.AssignStmt)
		if !ok {
			return true
		}
		for i, rhs := range as.Rhs {
			if i >= len(as.Lhs) {
				break
			}
			call, ok := rhs.(*ast.CallExpr)
			if !ok {
				continue
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok || !subContextFactories[sel.Sel.Name] {
				continue
			}
			id, ok := as.Lhs[i].(*ast.Ident)
			if ok {
				vars[id.Name] = true
			}
		}
		return true
	})
	return vars
}

// funcHasSink reports whether body (recursing into nested closures) contains a
// procedure-invocation sink: a call to ApplyCallable, or a no-arg .Run() on a
// sub-context local.
func funcHasSink(body ast.Node, subVars map[string]bool) bool {
	found := false
	ast.Inspect(body, func(n ast.Node) bool {
		if found {
			return false
		}
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		sel, ok := call.Fun.(*ast.SelectorExpr)
		if !ok {
			return true
		}
		if sel.Sel.Name == applyCallableSelector || procInvokingDriverSelectors[sel.Sel.Name] {
			found = true
			return false
		}
		if sel.Sel.Name == runSelector && len(call.Args) == 0 {
			id, ok := sel.X.(*ast.Ident)
			if ok && subVars[id.Name] {
				found = true
				return false
			}
		}
		return true
	})
	return found
}

// collectIntraPackageCallees returns the package-local callee names referenced in
// body, for both direct calls (foo()) and method/selector calls (x.foo()). Only
// names that are declared in the package (declNames) are kept; this resolves
// helper edges like a primitive calling a same-package helper that holds the sink.
func collectIntraPackageCallees(body ast.Node, declNames map[string]bool) []string {
	seen := map[string]bool{}
	var out []string
	add := func(name string) {
		if declNames[name] && !seen[name] {
			seen[name] = true
			out = append(out, name)
		}
	}
	ast.Inspect(body, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		switch fn := call.Fun.(type) {
		case *ast.Ident:
			add(fn.Name)
		case *ast.SelectorExpr:
			add(fn.Sel.Name)
		}
		return true
	})
	return out
}

// parsePackageFiles parses every non-test .go file directly in dir (non-recursive)
// and returns their ASTs. Used instead of the deprecated parser.ParseDir; build
// tags are irrelevant here because the primitive packages have none.
func parsePackageFiles(t *testing.T, fset *token.FileSet, dir string) []*ast.File {
	t.Helper()
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("read package dir %s: %v", dir, err)
	}
	var files []*ast.File
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		name := e.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		f, perr := parser.ParseFile(fset, filepath.Join(dir, name), nil, 0)
		if perr != nil {
			t.Fatalf("parse %s: %v", filepath.Join(dir, name), perr)
		}
		files = append(files, f)
	}
	return files
}

// splitQualified splits a runtime-qualified function name "<importpath>.<Func>"
// into its package import path and (possibly closure-suffixed) function name. The
// import path is the prefix up to the first '.' that follows the final '/'.
func splitQualified(qualified string) (pkg, fn string) {
	slash := strings.LastIndex(qualified, "/")
	rest := qualified[slash+1:]
	dot := strings.Index(rest, ".")
	if dot < 0 {
		return qualified, ""
	}
	return qualified[:slash+1+dot], rest[dot+1:]
}

// TestInvokesProcedureStaticGuard is the structural guard: it asserts that every
// registered primitive whose Impl transitively reaches a procedure-invocation sink
// carries InvokesProcedure:true. See the file header for the soundness model.
func TestInvokesProcedureStaticGuard(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
	if err != nil {
		t.Fatalf("new engine (profile=KitchenSink, no library paths): %v", err)
	}
	prims := eng.Registry().Primitives()

	type primInfo struct {
		name      string
		key       string
		annotated bool
	}
	pkgPaths := map[string]bool{}
	infos := make([]primInfo, 0, len(prims))
	for _, pr := range prims {
		goFn, _, _, rErr := resolveImpl(pr.Spec.Impl)
		if rErr != nil {
			t.Errorf("resolveImpl(%q): %v", pr.Spec.Name, rErr)
			continue
		}
		if goFn == "" {
			continue // binding-only primitive (nil Impl)
		}
		pkg, fn := splitQualified(goFn)
		fn = closureSuffix.ReplaceAllString(fn, "")
		if fn == "" {
			t.Errorf("primitive %q: cannot split qualified Impl name %q", pr.Spec.Name, goFn)
			continue
		}
		pkgPaths[pkg] = true
		infos = append(infos, primInfo{
			name:      pr.Spec.Name,
			key:       pkg + "." + fn,
			annotated: pr.Spec.InvokesProcedure,
		})
	}

	root := repoRoot(t)
	an := newProcInvokeAnalyzer()
	fset := token.NewFileSet()
	for pkg := range pkgPaths {
		relDir := strings.TrimPrefix(pkg, modulePath+"/")
		if relDir == pkg {
			// Primitive Impl outside this module — unexpected; skip rather than
			// fabricate a path. (No such primitive exists today.)
			t.Logf("skipping non-module primitive package %q", pkg)
			continue
		}
		dir := filepath.Join(root, relDir)
		files := parsePackageFiles(t, fset, dir)
		an.addPackage(pkg, files)
	}
	invokes := an.computeInvokes()

	// The soundness assertion: discovered ⇒ annotated.
	for _, in := range infos {
		if invokes[in.key] && !in.annotated {
			t.Errorf("primitive %q (Impl %s) transitively invokes a Scheme procedure "+
				"(reaches ApplyCallable or a sub-context Run) but is NOT marked "+
				"InvokesProcedure:true. The frame-reclaim classifier would wrongly trust "+
				"it as capture-safe and could release an env frame across a capturing "+
				"call. Add InvokesProcedure:true to its PrimitiveSpec (and a name to "+
				"TestInvokesProcedureCompleteness).", in.name, in.key)
		}
	}

	// Vacuity guard: if the analysis silently broke (e.g. a selector was renamed),
	// the soundness loop above would pass trivially. Pin a few primitives the
	// analyzer MUST discover, covering each sink shape: direct ApplyCallable
	// (PrimApply, PrimCallCC); the boundary reifications behind the RunBodyUnder*
	// drivers (PrimCallWithValues -> RunBodyUnderConsumer, PrimCallWithExit ->
	// RunBodyUnderExitFrame, PrimCallWithContinuationPrompt -> RunBodyUnderPrompt);
	// and a sub-context driver (PrimEval -> sub.RunWithinBoundary, the dataflow path,
	// no ApplyCallable). The RunBodyUnder*/RunWithinBoundary pins fail if the
	// procInvokingDriverSelectors set is dropped — the false-negative that the
	// reification would otherwise introduce.
	mustDiscover := []string{
		modulePath + "/pkg/registry/core.PrimApply",
		modulePath + "/pkg/registry/core.PrimCallCC",
		modulePath + "/pkg/registry/core.PrimCallWithValues",
		modulePath + "/pkg/registry/core.PrimCallWithExit",
		modulePath + "/pkg/registry/core.PrimCallWithContinuationPrompt",
		// (with-exception-handler was here; it is now Scheme — (parameterize over
		// %exception-handlers) — not a Go primitive. PrimRaise is not a valid
		// replacement pin: its handler invocation is indirect (via the value register
		// / RaiseInPlace), outside the analyzer's static call graph, same as the
		// pre-existing note above on raise.)
		modulePath + "/extensions/eval.PrimEval",
		// Pins the ExpandExpression/ExpandOnce selectors as load-bearing: dropping
		// them fails this row instead of silently reopening the hole they close.
		modulePath + "/extensions/eval.PrimExpandOnce",
	}
	for _, key := range mustDiscover {
		if !invokes[key] {
			t.Errorf("analyzer failed to discover known invoker %s — the static guard "+
				"is vacuous/broken (selector rename? package not analyzed?)", key)
		}
	}
}

// TestInvokesProcedureAnalyzerDetectsSinks exercises the discovery core on a
// synthetic package, proving the guard has teeth: an unannotated procedure-invoker
// is detected (direct, sub-context, and transitive), while exec.Cmd-style Run on a
// non-sub-context receiver is correctly NOT a sink. This is the
// intentionally-unannotated-fails demonstration the plan's "done-when" calls for.
func TestInvokesProcedureAnalyzerDetectsSinks(t *testing.T) {
	const pkg = "example.com/fake"
	const src = `
package fake

type ctx struct{}

func (ctx) ApplyCallable(args ...any) (any, error) { return nil, nil }
func (ctx) NewSubContext() *sub                     { return nil }

type sub struct{}

func (*sub) Run() error { return nil }

type cmd struct{}

func (cmd) Run() error { return nil }

// Directly invokes via ApplyCallable.
func PrimDirectApply(c ctx) {
	_, _ = c.ApplyCallable()
}

// Invokes via a sub-context Run (no ApplyCallable).
func PrimSubRun(c ctx) {
	s := c.NewSubContext()
	_ = s.Run()
}

// Transitive: calls a same-package helper that holds the sink.
func PrimTransitive(c ctx) {
	helper(c)
}

func helper(c ctx) {
	_, _ = c.ApplyCallable()
}

// NOT an invoker: exec.Cmd-style Run on a non-sub-context receiver.
func PrimRunsProcess(x cmd) {
	_ = x.Run()
}

// NOT an invoker: no sink at all.
func PrimPure() int {
	return 42
}
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "fake.go", src, 0)
	if err != nil {
		t.Fatalf("parse synthetic source: %v", err)
	}
	an := newProcInvokeAnalyzer()
	an.addPackage(pkg, []*ast.File{f})
	invokes := an.computeInvokes()

	tcs := []struct {
		fn   string
		want bool
	}{
		{"PrimDirectApply", true},
		{"PrimSubRun", true},
		{"PrimTransitive", true},
		{"helper", true},
		{"PrimRunsProcess", false}, // exec.Cmd.Run() must NOT register as a sink
		{"PrimPure", false},
	}
	for _, tc := range tcs {
		t.Run(tc.fn, func(t *testing.T) {
			got := invokes[pkg+"."+tc.fn]
			if got != tc.want {
				t.Errorf("invokes[%s] = %v, want %v", tc.fn, got, tc.want)
			}
		})
	}
}

// TestInvokesProcedureGuardModulePath pins the modulePath constant to go.mod, so a
// module rename can't silently break the join key that ties resolveImpl's
// runtime-qualified names to the AST analyzer's import-path keys.
func TestInvokesProcedureGuardModulePath(t *testing.T) {
	root := repoRoot(t)
	b, err := os.ReadFile(filepath.Join(root, "go.mod"))
	if err != nil {
		t.Fatalf("read go.mod: %v", err)
	}
	want := "module " + modulePath
	first := strings.SplitN(strings.TrimSpace(string(b)), "\n", 2)[0]
	if strings.TrimSpace(first) != want {
		t.Errorf("go.mod module line = %q, want %q (update modulePath)", first, want)
	}
}
