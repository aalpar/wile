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
// that is refused; emitCachedBindingLoad applies it for the three arms of
// CompileSymbol that end at a compile-time-resolved global, the library-scope
// arm applies it directly because it must keep runtime resolution, and
// tryResolvedBinding applies it for the classification on its two pin arms.
// Nothing in the type system says any of them has to. The library arm did not,
// for the whole life of that arm: a library body's (list if) evaluated to
// (#<primitive-expander:if>). This file is what makes the next such arm fail CI
// instead of leaking.
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
// SOUNDNESS MODEL — and the direction matters, because the obvious statement of
// it is backwards. The verdict this file computes is "GUARDED", and guarded must
// be UNDER-approximated: a false "guarded" is a silent leak, a false "leaks" is
// a loud CI failure someone fixes. So every judgement is made the strict way:
//
//   - The arm is the emit's OWN statement list, and the refusal must appear at a
//     statement that lexically PRECEDES the emit in it. An arm that emits first
//     and classifies afterwards is not guarded, however loudly it names the
//     refusal; neither is one vouched for only by a sibling arm's refusal.
//   - The refusal must be reached on EVERY path through that statement: in its
//     unconditional part (an if's condition, not its branches), and through
//     helpers that refuse on every path through them. A refusal on another
//     branch guards only that branch; the library arm's `if imported { return
//     p.emitCachedBindingLoad(...) }` once vouched for its non-imported path.
//     The cost is explicit exemptions for the nil-binding fallbacks
//     (nilBindingFallbacks), which have no binding to refuse.
//   - The verdict must be PROPAGATED: a `return` has to sit between the refusal
//     and the emit. `_ = refuseCompileTimeMeaning(...)` guards nothing.
//   - Helpers count. Arms are collected from CompileSymbol AND from every
//     package-local function it transitively calls, because an arm whose emit
//     lives in a helper is the shape tryResolvedBinding already uses and is
//     therefore the likely way the next one gets written. Restricting discovery
//     to CompileSymbol's own FuncDecl let `if p.emitLeakyArm(sym) { return nil }`
//     pass while leaking.
//
// Residual holes, stated rather than assumed away — each is a place the analysis
// over-approximates "guarded" and so could miss a leak:
//   - The call graph is NAME-keyed, so two package-local declarations sharing a
//     name collapse into one node. Interface-satisfying methods in this package
//     do repeat (EqualTo, SchemeString, Apply, …) and are harmless;
//     TestCompileSymbolRefusalGuardNamesAreUnique fails if one of the four names
//     the guard turns on ever gains a twin, rather than leaving that as an
//     assumption.
//   - A refusal whose result is discarded still counts when some unrelated
//     `return` happens to sit between it and the emit.
//   - Interface and function-value calls are invisible; an emit reached only
//     through one is not discovered.

package compilation

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"sort"
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
	"NewOperationLoadCachedBinding":                            true,
	"NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate": true,
}

// refusalReach is the package's name-keyed call graph, reduced to the set of
// functions that transitively reach refuseCompileTimeMeaning.
//
// edges is every call in a body, for discovering the functions CompileSymbol
// reaches; mustEdges is only the calls a body makes on EVERY path through it
// (unconditionalCallees), for deciding which functions are a guard.
type refusalReach struct {
	edges     map[string][]string
	mustEdges map[string][]string
	names     map[string]bool
	decls     map[string]*ast.FuncDecl
}

func newRefusalReach() *refusalReach {
	return &refusalReach{
		edges:     map[string][]string{},
		mustEdges: map[string][]string{},
		names:     map[string]bool{},
		decls:     map[string]*ast.FuncDecl{},
	}
}

// addFiles records every top-level function declaration in files and the
// package-local functions it calls. Method and plain calls are both keyed on
// the bare name; the names that keying is load-bearing for are checked for
// twins by TestCompileSymbolRefusalGuardNamesAreUnique.
func (p *refusalReach) addFiles(files []*ast.File) {
	for _, f := range files {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if !ok {
				continue
			}
			p.names[fd.Name.Name] = true
			if fd.Body != nil {
				p.decls[fd.Name.Name] = fd
			}
		}
	}
	for name, fd := range p.decls {
		p.edges[name] = calleeNames(fd.Body)
		p.mustEdges[name] = unconditionalCallees(fd.Body.List)
	}
}

// compute returns the set of function names that reach the refusal on every
// path through them, iterated to a fixed point. A function that refuses only on
// some branch (tryResolvedBinding refuses a pin that resolves, not one that does
// not) is not a guard for the statement that calls it.
func (p *refusalReach) compute() map[string]bool {
	reaches := map[string]bool{refusalFunc: true}
	for {
		changed := false
		for name, callees := range p.mustEdges {
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

// calledFrom returns root plus every package-local function reachable from it,
// in sorted order so the report is stable.
func (p *refusalReach) calledFrom(root string) []string {
	seen := map[string]bool{}
	queue := []string{root}
	for len(queue) > 0 {
		cur := queue[0]
		queue = queue[1:]
		if seen[cur] {
			continue
		}
		seen[cur] = true
		for _, c := range p.edges[cur] {
			if p.decls[c] != nil && !seen[c] {
				queue = append(queue, c)
			}
		}
	}
	out := make([]string, 0, len(seen))
	for n := range seen {
		if p.decls[n] != nil {
			out = append(out, n)
		}
	}
	sort.Strings(out)
	return out
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

// unconditionalCallees returns the names called on every path through list: the
// calls in each statement's unconditional part (stmtUnconditionalCallees), up to
// and including the first statement that can return. A call after a possible
// return is skipped on that return's path.
func unconditionalCallees(list []ast.Stmt) []string {
	var out []string
	for _, st := range list {
		out = append(out, stmtUnconditionalCallees(st)...)
		if stmtReturns(st) {
			break
		}
	}
	return out
}

// stmtUnconditionalCallees returns the names st calls whenever it executes: an
// if's init and condition but not its branches, a block's own list, and no
// loop, switch or select body, function literal, or right operand of && or ||.
func stmtUnconditionalCallees(st ast.Stmt) []string {
	switch x := st.(type) {
	case *ast.BlockStmt:
		return unconditionalCallees(x.List)
	case *ast.IfStmt:
		var out []string
		if x.Init != nil {
			out = append(out, stmtUnconditionalCallees(x.Init)...)
		}
		return append(out, exprUnconditionalCallees(x.Cond)...)
	case *ast.ExprStmt:
		return exprUnconditionalCallees(x.X)
	case *ast.AssignStmt:
		var out []string
		for _, e := range x.Rhs {
			out = append(out, exprUnconditionalCallees(e)...)
		}
		return out
	case *ast.ReturnStmt:
		var out []string
		for _, e := range x.Results {
			out = append(out, exprUnconditionalCallees(e)...)
		}
		return out
	case *ast.DeclStmt:
		var out []string
		ast.Inspect(x, func(n ast.Node) bool {
			e, ok := n.(ast.Expr)
			if !ok {
				return true
			}
			out = append(out, exprUnconditionalCallees(e)...)
			return false
		})
		return out
	}
	return nil
}

// exprUnconditionalCallees returns the names e calls whenever it is evaluated.
func exprUnconditionalCallees(e ast.Expr) []string {
	var out []string
	ast.Inspect(e, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncLit:
			return false
		case *ast.BinaryExpr:
			if x.Op == token.LAND || x.Op == token.LOR {
				out = append(out, exprUnconditionalCallees(x.X)...)
				return false
			}
		case *ast.CallExpr:
			name := calleeName(x)
			if name != "" {
				out = append(out, name)
			}
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

// emitSite is one global-binding load: the function it lives in, and the
// INNERMOST statement list enclosing it with the index of the statement that
// contains it. The refusal has to appear earlier in that same list.
//
// Innermost only, deliberately. Consulting enclosing lists as well would let a
// SIBLING arm's refusal vouch for this one — every arm of CompileSymbol is an
// if at the same level, so one guarded arm would mark all of them guarded. A
// guard that an outer level does supply and this misses is a false positive:
// loud, and fixed by moving the refusal into the arm.
type emitSite struct {
	fn   string
	pos  token.Pos
	list []ast.Stmt
	idx  int
}

// collectEmits returns every global-load emit in fd, each anchored to the
// statement list that encloses it.
func collectEmits(name string, fd *ast.FuncDecl) []emitSite {
	var out []emitSite
	collectEmitsInList(fd.Body.List, name, &out)
	return out
}

func collectEmitsInList(list []ast.Stmt, fnName string, out *[]emitSite) {
	for i, st := range list {
		blk, ok := st.(*ast.BlockStmt)
		if ok {
			collectEmitsInList(blk.List, fnName, out)
			continue
		}
		collectEmitsInStmt(st, list, i, fnName, out)
	}
}

// collectEmitsInStmt scans one statement for global-load calls, handing any
// nested statement list back to collectEmitsInList so each emit is anchored to
// the block it is really in rather than to the traversal root.
func collectEmitsInStmt(st ast.Stmt, list []ast.Stmt, idx int, fnName string, out *[]emitSite) {
	ast.Inspect(st, func(n ast.Node) bool {
		if n == nil {
			return false
		}
		if n != ast.Node(st) {
			switch b := n.(type) {
			case *ast.BlockStmt:
				collectEmitsInList(b.List, fnName, out)
				return false
			case *ast.CaseClause:
				collectEmitsInList(b.Body, fnName, out)
				return false
			case *ast.CommClause:
				collectEmitsInList(b.Body, fnName, out)
				return false
			}
		}
		call, ok := n.(*ast.CallExpr)
		if ok && globalLoadOps[calleeName(call)] {
			*out = append(*out, emitSite{fn: fnName, pos: call.Pos(), list: list, idx: idx})
		}
		return true
	})
}

// emitIsGuarded reports whether the emit's own statement list applies the
// refusal at a statement BEFORE the emit and propagates its verdict with a
// return between the two.
func emitIsGuarded(site emitSite, reaches map[string]bool) bool {
	refusalAt := -1
	for j := range site.idx {
		if stmtReachesRefusal(site.list[j], reaches) {
			refusalAt = j
			break
		}
	}
	if refusalAt < 0 {
		return false
	}
	for k := refusalAt; k < site.idx; k++ {
		if stmtReturns(site.list[k]) {
			return true
		}
	}
	return false
}

// nilBindingFallbacks are the emits exempt from emitIsGuarded, by function and
// count: each is the runtime-resolution fallback for a global index whose
// binding is nil at compile time, so there is no binding to classify and the
// refusal would be a no-op (refuseCompileTimeMeaning: a nil binding is not a
// refusal). The count is exact, so a new fallback, or one that stops being
// needed, fails the ratchet instead of riding an existing exemption.
var nilBindingFallbacks = map[string]int{
	// The empty-scope arm: GetGlobalBinding(gi) is nil.
	compileSymbolFunc: 1,
	// The pin arm: GetOwnGlobalBinding(gi) is nil.
	"tryResolvedBinding": 1,
}

// isNilBindingFallback reports whether the emit's own list first handles the
// non-nil case: an earlier `if x != nil { ... }` with no else, whose body
// reaches the refusal on every path and returns. The emit is then the nil
// branch of a binding that is refused whenever there is one.
func isNilBindingFallback(site emitSite, reaches map[string]bool) bool {
	for j := range site.idx {
		ifs, ok := site.list[j].(*ast.IfStmt)
		if !ok || ifs.Else != nil || ifs.Init != nil {
			continue
		}
		cond, ok := ifs.Cond.(*ast.BinaryExpr)
		if !ok || cond.Op != token.NEQ {
			continue
		}
		nilID, ok := cond.Y.(*ast.Ident)
		if !ok || nilID.Name != "nil" {
			continue
		}
		if !stmtReturns(ifs.Body) {
			continue
		}
		for _, name := range unconditionalCallees(ifs.Body.List) {
			if reaches[name] {
				return true
			}
		}
	}
	return false
}

// stmtReachesRefusal reports whether st, whenever it executes, calls something
// that reaches the refusal on every path. A refusal inside one of st's branches
// does not count: in CompileSymbol's library arm, `if imported { return
// p.emitCachedBindingLoad(...) }` refuses on the imported path only, and
// counting it let the non-imported path drop its own refusal unseen.
func stmtReachesRefusal(st ast.Stmt, reaches map[string]bool) bool {
	for _, name := range stmtUnconditionalCallees(st) {
		if reaches[name] {
			return true
		}
	}
	return false
}

// stmtReturns reports whether st contains a return of its OWN function. A
// return inside a nested function literal returns from the literal and
// propagates nothing, so the walk stops at one.
func stmtReturns(st ast.Stmt) bool {
	found := false
	ast.Inspect(st, func(n ast.Node) bool {
		if found {
			return false
		}
		_, isLit := n.(*ast.FuncLit)
		if isLit {
			return false
		}
		_, isRet := n.(*ast.ReturnStmt)
		if isRet {
			found = true
			return false
		}
		return true
	})
	return found
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

// collectSurfaceEmits returns every global-load emit in CompileSymbol and in the
// package-local functions it transitively calls.
func collectSurfaceEmits(reach *refusalReach) []emitSite {
	var sites []emitSite
	for _, name := range reach.calledFrom(compileSymbolFunc) {
		sites = append(sites, collectEmits(name, reach.decls[name])...)
	}
	return sites
}

// TestCompileSymbolRefusesCompileTimeMeaningOnEveryGlobalArm is the guard: every
// arm that emits a load of a GLOBAL binding, in CompileSymbol or in anything it
// calls, must apply refuseCompileTimeMeaning before the emit and return on its
// verdict.
func TestCompileSymbolRefusesCompileTimeMeaningOnEveryGlobalArm(t *testing.T) {
	fset := token.NewFileSet()
	files := parsePackageDir(t, fset, ".")

	reach := newRefusalReach()
	reach.addFiles(files)
	if !reach.names[refusalFunc] {
		t.Fatalf("%s is not declared in this package — the guard has been "+
			"renamed out from under its own sink and is now vacuous", refusalFunc)
	}
	if reach.decls[compileSymbolFunc] == nil {
		t.Fatalf("%s not found — the guard cannot analyze what it cannot locate", compileSymbolFunc)
	}
	reaches := reach.compute()

	sites := collectSurfaceEmits(reach)
	// Vacuity guards. If the operation constructors are renamed, or the arms are
	// restructured so no emit is discovered, the loop below passes trivially.
	// Five emits exist today: two in CompileSymbol (the empty-scope runtime
	// fallback and the library arm's non-imported fallback), two in
	// tryResolvedBinding, one in emitCachedBindingLoad.
	if len(sites) < 3 {
		t.Fatalf("discovered %d global-load emit(s) reachable from %s, want at "+
			"least 3 — the analysis is broken (operation constructor renamed? "+
			"arms flattened?)", len(sites), compileSymbolFunc)
	}
	offCompileSymbol := 0
	for _, s := range sites {
		if s.fn != compileSymbolFunc {
			offCompileSymbol++
		}
	}
	if offCompileSymbol == 0 {
		t.Fatalf("every discovered emit is inside %s itself — the helper-following "+
			"half of the analysis is not running, which is exactly the hole that "+
			"let an emit in a helper pass", compileSymbolFunc)
	}

	exempt := map[string]int{}
	for _, s := range sites {
		if emitIsGuarded(s, reaches) {
			continue
		}
		if nilBindingFallbacks[s.fn] > 0 && isNilBindingFallback(s, reaches) {
			exempt[s.fn]++
			continue
		}
		t.Errorf("%s: the global load at %s emits a load of a resolved global "+
			"without applying %s before it and returning on the result. A binding "+
			"whose type is not BindingTypeVariable is a compile-time meaning (a "+
			"primitive expander or a syntax compiler), and emitting a load of one "+
			"leaks it into the value world (R7RS §4.1.1/§4.3.1). Either route the "+
			"arm through emitCachedBindingLoad, or call %s directly — before the "+
			"emit, and return its error — when the arm must keep runtime resolution.",
			s.fn, fset.Position(s.pos), refusalFunc, refusalFunc)
	}
	for fn, want := range nilBindingFallbacks {
		if exempt[fn] != want {
			t.Errorf("%s: %d nil-binding fallback emit(s) exempted, want %d — a "+
				"fallback was added or removed; update nilBindingFallbacks only after "+
				"checking the new emit cannot see a non-nil binding", fn, exempt[fn], want)
		}
	}
}

// TestCompileSymbolRefusalGuardNamesAreUnique checks the assumption the
// name-keyed call graph makes, for the names the guard actually turns on.
// Interface-satisfying methods (EqualTo, SchemeString, Apply, …) legitimately
// repeat across types in this package and are harmless; a second declaration of
// one of the four names below would merge callee sets and let one inherit the
// other's refusal, which is the direction that silently marks an arm guarded.
func TestCompileSymbolRefusalGuardNamesAreUnique(t *testing.T) {
	fset := token.NewFileSet()
	files := parsePackageDir(t, fset, ".")
	counts := map[string]int{}
	for _, f := range files {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if ok {
				counts[fd.Name.Name]++
			}
		}
	}
	loadBearing := []string{
		compileSymbolFunc,
		refusalFunc,
		"emitCachedBindingLoad",
		"tryResolvedBinding",
	}
	for _, name := range loadBearing {
		if counts[name] != 1 {
			t.Errorf("%s is declared %d times, want exactly 1; the reachability "+
				"guard keys its call graph on the bare name, so two same-named "+
				"declarations merge and one inherits the other's refusal",
				name, counts[name])
		}
	}
}

// TestCompileSymbolRefusalAnalyzerHasTeeth exercises the discovery core on
// synthetic source. Each leaky shape below is one the real analysis has to
// catch, and the first three are shapes the block-subtree version of this guard
// passed.
func TestCompileSymbolRefusalAnalyzerHasTeeth(t *testing.T) {
	const src = `
package fake

func refuseCompileTimeMeaning(a, b int) error { return nil }

func emitCachedBindingLoad(a, b int) error {
	err := refuseCompileTimeMeaning(a, b)
	if err != nil {
		return err
	}
	p.AppendOperations(machine.NewOperationLoadCachedBinding(0))
	return nil
}

func emitLeakyHelperArm(a int) error {
	p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
	return nil
}

func refusesOnOneBranch(a int) error {
	if a > 0 {
		return refuseCompileTimeMeaning(a, a)
	}
	return nil
}

func CompileSymbol() error {
	if guarded {
		return emitCachedBindingLoad(1, 2)
	}
	if directlyGuarded {
		err := refuseCompileTimeMeaning(1, 2)
		if err != nil {
			return err
		}
		p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
		return nil
	}
	if viaHelper {
		return emitLeakyHelperArm(1)
	}
	if emitsThenClassifies {
		p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
		_ = refuseCompileTimeMeaning(1, 2)
		return nil
	}
	if dropsTheError {
		_ = refuseCompileTimeMeaning(1, 2)
		p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
		return nil
	}
	if leaky {
		p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
		return nil
	}
	if refusedOnSiblingBranch {
		if imported {
			return emitCachedBindingLoad(1, 2)
		}
		p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
		return nil
	}
	if refusedByConditionalHelper {
		err := refusesOnOneBranch(1)
		if err != nil {
			return err
		}
		p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
		return nil
	}
	if refusedInCondition {
		if refuseCompileTimeMeaning(1, 2) != nil {
			return nil
		}
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

	sites := collectSurfaceEmits(reach)
	// Nine global loads across the surface: seven in CompileSymbol
	// (directlyGuarded, emitsThenClassifies, dropsTheError, leaky,
	// refusedOnSiblingBranch, refusedByConditionalHelper, refusedInCondition),
	// one in emitCachedBindingLoad, one in emitLeakyHelperArm. The LOCAL load is
	// not a global arm and must not be counted.
	if len(sites) != 9 {
		t.Fatalf("discovered %d emits, want 9 (the LOCAL load is not a global arm)", len(sites))
	}
	if reaches["refusesOnOneBranch"] {
		t.Fatal("refusesOnOneBranch refuses on one branch only and must not count as a guard")
	}
	unguarded := map[string]bool{}
	for _, s := range sites {
		if emitIsGuarded(s, reaches) {
			continue
		}
		unguarded[fset.Position(s.pos).String()] = true
	}
	if len(unguarded) != 6 {
		t.Errorf("unguarded emits = %v, want exactly six (viaHelper's helper, "+
			"emitsThenClassifies, dropsTheError, leaky, refusedOnSiblingBranch, "+
			"refusedByConditionalHelper) — the analyzer either misses one, which is "+
			"the silent hole this guard exists to close, or falsely flags a guarded "+
			"arm", unguarded)
	}
}

// TestNilBindingFallbackExemptionIsStructural pins isNilBindingFallback on
// synthetic arms: the exemption needs an earlier non-nil branch that refuses on
// every path and returns, not merely a nil check.
func TestNilBindingFallbackExemptionIsStructural(t *testing.T) {
	const src = `
package fake

func refuseCompileTimeMeaning(a, b int) error { return nil }

func refusedThenFallback() {
	if bd != nil {
		return refuseCompileTimeMeaning(1, 2)
	}
	p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
}

func nilCheckWithoutRefusal() {
	if bd != nil {
		return nil
	}
	p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
}

func refusalWithoutReturn() {
	if bd != nil {
		_ = refuseCompileTimeMeaning(1, 2)
	}
	p.AppendOperations(machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0))
}
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "fake.go", src, 0)
	if err != nil {
		t.Fatalf("parse synthetic source: %v", err)
	}
	reach := newRefusalReach()
	reach.addFiles([]*ast.File{f})
	reaches := reach.compute()
	want := map[string]bool{
		"refusedThenFallback":    true,
		"nilCheckWithoutRefusal": false,
		"refusalWithoutReturn":   false,
	}
	for fn, exempt := range want {
		sites := collectEmits(fn, reach.decls[fn])
		if len(sites) != 1 {
			t.Fatalf("%s: discovered %d emits, want 1", fn, len(sites))
		}
		got := isNilBindingFallback(sites[0], reaches)
		if got != exempt {
			t.Errorf("%s: isNilBindingFallback = %v, want %v", fn, got, exempt)
		}
	}
}
