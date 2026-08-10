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

// Static structural guard for CompileSymbol's syntactic-keyword refusal.
//
// A syntactic keyword is not a variable (R7RS §4.1.1, §4.3.1): emitting a load
// of a binding whose type is not BindingTypeVariable hands a primitive expander
// or a syntax compiler to Scheme as a value. refuseCompileTimeMeaning is where
// that is refused, and emitCachedBindingLoad applies it for the three arms of
// CompileSymbol that end at a compile-time-resolved global.
//
// The FOURTH arm — library scope — cannot route through emitCachedBindingLoad:
// it must keep runtime resolution, because the library template has not run
// when the reference compiles. It therefore applies the refusal directly, and
// nothing in the type system says it has to. It did not, for the whole life of
// that arm: a library body's (list if) evaluated to (#<primitive-expander:if>).
// This file is what makes a fifth arm fail CI instead of leaking.
//
// Why AST and not ruleguard or x/tools SSA — the same two reasons
// pkg/wile/invokes_procedure_guard_test.go gives, and this file is modelled on
// that one:
//   - ruleguard matches syntax within a single function; it cannot follow the
//     emitCachedBindingLoad -> refuseCompileTimeMeaning edge.
//   - x/tools (SSA + callgraph) is the textbook tool, but the module's go.mod is
//     deliberately minimal (CLAUDE.md: prefer the standard library). The analysis
//     needed here fits in go/ast + go/parser.
//
// Soundness model, and it is the same one: OVER-approximate "reaches". A false
// positive demands a refusal on an arm that did not need one — a loud, fixable
// CI failure. A false negative is the silent leak this exists to prevent. So
// "reaches" is resolved by name, transitively, across the whole package, and an
// arm counts as guarded when the refusal appears anywhere in the enclosing
// block's subtree.
//
// The known hole, stated rather than assumed away: the arm granularity is the
// INNERMOST enclosing block. An emit added directly in CompileSymbol's own body
// block is checked against that block, which already contains
// emitCachedBindingLoad calls, so it would pass. Every arm today is nested in an
// if, and TestCompileSymbolRefusalAnalyzerHasTeeth pins that a nested unguarded
// one is caught; a body-level one is not covered here.

package compilation

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

const (
	// compileSymbolFunc is the function whose arms are under audit.
	compileSymbolFunc = "CompileSymbol"
	// refusalFunc is the sink: the shared syntactic-keyword refusal.
	refusalFunc = "refuseCompileTimeMeaning"
)

// globalLoadOps are the machine operation constructors that emit a load of a
// GLOBAL binding — the two shapes a resolved global can reach the VM through.
// Local loads (NewOperationLoadLocalByLocalIndexImmediate) are deliberately
// absent: a local slot cannot hold a compile-time meaning, and including it
// would make every lexical arm demand a refusal it has nothing to apply it to.
var globalLoadOps = map[string]bool{
	"NewOperationLoadCachedBinding":                           true,
	"NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate": true,
}

// refusalReach is the package's name-keyed call graph, reduced to the set of
// functions that transitively reach refuseCompileTimeMeaning.
type refusalReach struct {
	edges map[string][]string
	names map[string]bool
}

func newRefusalReach() *refusalReach {
	return &refusalReach{
		edges: map[string][]string{},
		names: map[string]bool{},
	}
}

// addFiles records every top-level function declaration in files and the
// package-local functions it calls. Method and plain calls are both keyed on
// the bare name: this package has no two declarations sharing one name, and
// collapsing them is the over-approximating direction anyway.
func (p *refusalReach) addFiles(files []*ast.File) {
	for _, f := range files {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if !ok {
				continue
			}
			p.names[fd.Name.Name] = true
		}
	}
	for _, f := range files {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if !ok || fd.Body == nil {
				continue
			}
			p.edges[fd.Name.Name] = calleeNames(fd.Body)
		}
	}
}

// compute returns the set of function names that reach the refusal, iterated to
// a fixed point.
func (p *refusalReach) compute() map[string]bool {
	reaches := map[string]bool{refusalFunc: true}
	for {
		changed := false
		for name, callees := range p.edges {
			if reaches[name] {
				continue
			}
			for _, c := range callees {
				if reaches[c] {
					reaches[name] = true
					changed = true
					break
				}
			}
		}
		if !changed {
			break
		}
	}
	return reaches
}

// calleeNames returns the bare names of every function called in body, for both
// direct calls (foo()) and selector calls (x.foo()).
func calleeNames(body ast.Node) []string {
	seen := map[string]bool{}
	var out []string
	ast.Inspect(body, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		name := calleeName(call)
		if name != "" && !seen[name] {
			seen[name] = true
			out = append(out, name)
		}
		return true
	})
	return out
}

// calleeName reduces a call expression to the bare name of what it calls, or ""
// when the callee is not a plain identifier or selector.
func calleeName(call *ast.CallExpr) string {
	switch fn := call.Fun.(type) {
	case *ast.Ident:
		return fn.Name
	case *ast.SelectorExpr:
		return fn.Sel.Name
	}
	return ""
}

// globalLoadArms returns, for each global-load emit inside fd, the innermost
// enclosing block statement — the "arm" the emit belongs to — paired with the
// emit's position for the diagnostic.
//
// The walk is hand-rolled rather than an ast.Inspect with a stack: Inspect
// reports the leaving edge (a nil node) only for subtrees the visitor descended
// into, so a stack popped on nil pops for the wrong nodes.
func globalLoadArms(fd *ast.FuncDecl) ([]*ast.BlockStmt, []token.Pos) {
	var blocks []*ast.BlockStmt
	var positions []token.Pos
	walkWithEnclosingBlock(fd.Body, nil, func(call *ast.CallExpr, blk *ast.BlockStmt) {
		if !globalLoadOps[calleeName(call)] {
			return
		}
		blocks = append(blocks, blk)
		positions = append(positions, call.Pos())
	})
	return blocks, positions
}

// walkWithEnclosingBlock calls visit for every call expression under n, passing
// the innermost *ast.BlockStmt enclosing it. cur seeds the enclosing block for
// n itself.
func walkWithEnclosingBlock(n ast.Node, cur *ast.BlockStmt, visit func(*ast.CallExpr, *ast.BlockStmt)) {
	if n == nil {
		return
	}
	blk, isBlock := n.(*ast.BlockStmt)
	if isBlock {
		cur = blk
	}
	call, isCall := n.(*ast.CallExpr)
	if isCall && cur != nil {
		visit(call, cur)
	}
	// Descend exactly one level, then recurse by hand so cur tracks the block
	// nesting instead of the traversal order.
	ast.Inspect(n, func(child ast.Node) bool {
		if child == n {
			return true
		}
		if child == nil {
			return false
		}
		walkWithEnclosingBlock(child, cur, visit)
		return false
	})
}

// armReachesRefusal reports whether any call in blk's subtree lands in the
// reaches-refusal set.
func armReachesRefusal(blk *ast.BlockStmt, reaches map[string]bool) bool {
	found := false
	ast.Inspect(blk, func(n ast.Node) bool {
		if found {
			return false
		}
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		if reaches[calleeName(call)] {
			found = true
			return false
		}
		return true
	})
	return found
}

// findFunc returns the FuncDecl named name, or nil.
func findFunc(files []*ast.File, name string) *ast.FuncDecl {
	for _, f := range files {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if ok && fd.Name.Name == name && fd.Body != nil {
				return fd
			}
		}
	}
	return nil
}

// parsePackageDir parses every non-test .go file directly in dir. Used instead
// of the deprecated parser.ParseDir; this package carries no build tags.
func parsePackageDir(t *testing.T, fset *token.FileSet, dir string) []*ast.File {
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

// TestCompileSymbolRefusesCompileTimeMeaningOnEveryGlobalArm is the guard: every
// arm of CompileSymbol that emits a load of a GLOBAL binding must reach
// refuseCompileTimeMeaning, directly or through emitCachedBindingLoad.
func TestCompileSymbolRefusesCompileTimeMeaningOnEveryGlobalArm(t *testing.T) {
	fset := token.NewFileSet()
	files := parsePackageDir(t, fset, ".")

	reach := newRefusalReach()
	reach.addFiles(files)
	if !reach.names[refusalFunc] {
		t.Fatalf("%s is not declared in this package — the guard has been "+
			"renamed out from under its own sink and is now vacuous", refusalFunc)
	}
	reaches := reach.compute()

	fd := findFunc(files, compileSymbolFunc)
	if fd == nil {
		t.Fatalf("%s not found — the guard cannot analyze what it cannot locate", compileSymbolFunc)
	}

	blocks, positions := globalLoadArms(fd)
	// Vacuity guard. If the operation constructors are renamed, or the arms are
	// restructured so no emit is nested, the loop below passes trivially.
	// Two arms emit a global load in CompileSymbol today: the empty-scope
	// runtime fallback and the library-scope arm.
	if len(blocks) < 2 {
		t.Fatalf("discovered %d global-load arm(s) in %s, want at least 2 — the "+
			"analysis is broken (operation constructor renamed? arms flattened?)",
			len(blocks), compileSymbolFunc)
	}

	for i, blk := range blocks {
		if armReachesRefusal(blk, reaches) {
			continue
		}
		t.Errorf("%s: the arm containing the global load at %s emits a load of a "+
			"resolved global without reaching %s. A binding whose type is not "+
			"BindingTypeVariable is a compile-time meaning (a primitive expander "+
			"or a syntax compiler), and emitting a load of one leaks it into the "+
			"value world (R7RS §4.1.1/§4.3.1). Either route the arm through "+
			"emitCachedBindingLoad, or call %s directly when the arm must keep "+
			"runtime resolution.",
			compileSymbolFunc, fset.Position(positions[i]), refusalFunc, refusalFunc)
	}
}

// TestCompileSymbolRefusalAnalyzerHasTeeth exercises the discovery core on
// synthetic source, proving the guard would actually catch a new unguarded arm:
// an arm that emits a global load with no refusal in reach is flagged, one that
// goes through a transitive helper is not, and a LOCAL load is not an arm at
// all.
func TestCompileSymbolRefusalAnalyzerHasTeeth(t *testing.T) {
	const src = `
package fake

func refuseCompileTimeMeaning(a, b int) error { return nil }

func emitCachedBindingLoad(a, b int) error {
	if err := refuseCompileTimeMeaning(a, b); err != nil {
		return err
	}
	p.AppendOperations(machine.NewOperationLoadCachedBinding(0))
	return nil
}

func CompileSymbol() error {
	if guarded {
		return emitCachedBindingLoad(1, 2)
	}
	if directlyGuarded {
		if err := refuseCompileTimeMeaning(1, 2); err != nil {
			return err
		}
		p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
		return nil
	}
	if leaky {
		p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
		return nil
	}
	if local {
		p.AppendOperations(machine.NewOperationLoadLocalByLocalIndexImmediate(0))
		return nil
	}
	return nil
}
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "fake.go", src, 0)
	if err != nil {
		t.Fatalf("parse synthetic source: %v", err)
	}
	files := []*ast.File{f}

	reach := newRefusalReach()
	reach.addFiles(files)
	reaches := reach.compute()
	if !reaches["emitCachedBindingLoad"] {
		t.Fatal("emitCachedBindingLoad must be discovered as reaching the refusal " +
			"— without the transitive edge the real guard flags every cached arm")
	}

	blocks, positions := globalLoadArms(findFunc(files, compileSymbolFunc))
	// emitCachedBindingLoad's own emit is outside CompileSymbol, so only the
	// directlyGuarded and leaky arms are discovered. The local arm must NOT be.
	if len(blocks) != 2 {
		t.Fatalf("discovered %d arms, want 2 (the guarded and the leaky global "+
			"loads; the LOCAL load is not a global arm)", len(blocks))
	}
	var unguarded []string
	for i, blk := range blocks {
		if armReachesRefusal(blk, reaches) {
			continue
		}
		unguarded = append(unguarded, fset.Position(positions[i]).String())
	}
	if len(unguarded) != 1 {
		t.Errorf("unguarded arms = %v, want exactly one (the `leaky` arm) — the "+
			"analyzer either misses it, which is the silent hole this guard "+
			"exists to close, or falsely flags a guarded arm", unguarded)
	}
}
