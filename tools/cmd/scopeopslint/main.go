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

// Command scopeopslint counts raw slice operations on a hygiene scope set.
//
// It is the ratchet that keeps the tree from drifting back to the slice
// representation a scope set used to have. Until 2026-09 a scope set was
// []*syntax.Scope / []*values.Scope, and every touch copied it; it is now the
// opaque values.Scopes persistent chain, which deliberately exposes no Slice()
// and no indexed access, because member ORDER is an implementation invariant
// rather than part of the contract (pkg/values/scopes.go). Nothing in the type
// system prevents a caller from reintroducing a []*Scope beside it, so the
// prohibition is measured instead.
//
// # Method: expression-driven, not statement-driven
//
// Enumerate every expression that go/types assigns a scope-slice type, then ask
// what its PARENT node does to it. The operation is recorded against the parent
// (or against the expression itself, for a composite literal). Going through
// types.Info.Types finds operations reachable through named types, struct
// fields, func results and type parameters for free — a pattern-matching walk
// over source spellings finds only the ones spelled []*Scope at the site.
//
// # The alias trap
//
// syntax.Scope is a type ALIAS of values.Scope (pkg/syntax/syntax_value.go).
// Without types.Unalias the element type of []*syntax.Scope is a *types.Alias,
// not the *types.Named the matcher is looking for, so every site outside
// pkg/values goes silently uncounted: at the pre-flip commit d2927bea, 118
// operations over 11 files instead of 423 over 49, out of 13 files holding any
// scope-slice-typed expression instead of 91. The walk applies types.Unalias by
// default; -noalias turns it off, and exists precisely so the difference stays
// auditable rather than folkloric. A walk that reports a suspiciously small
// number was probably run without it.
//
// # What counts as an operation
//
// len, cap, range, index, slice expression, append (in either position), copy,
// clear, make, new, a conversion to or from the slice type, a composite literal
// (INCLUDING AN EMPTY ONE — omitting empties is what made an earlier walk read
// 408 instead of 423), a comparison against nil, and use as an argument to a
// slices.*, sort.* or reflect.* helper. Merely reading, passing or storing the
// value is not an operation: the point is the representation being manipulated,
// not the set being used.
//
// A package load or type error is FATAL. A walk over a tree that does not
// typecheck sees no expressions and would report zero, which is the one failure
// mode a ratchet must never have.
//
// Usage:
//
//	go run ./tools/cmd/scopeopslint                                   # report
//	go run ./tools/cmd/scopeopslint -json                             # sites as JSON
//	go run ./tools/cmd/scopeopslint -check testdata/scope-ops-manifest.scm
//	go run ./tools/cmd/scopeopslint -write testdata/scope-ops-manifest.scm
//	go run ./tools/cmd/scopeopslint -tests=false -noalias             # the controls
//
// -check exits non-zero when any file carries more operations than the manifest
// grants it (a file absent from the manifest is granted zero). It is a ceiling,
// not an equality: removing operations passes, and the manifest is then ratcheted
// down by regenerating it.
package main

import (
	"cmp"
	"encoding/json"
	"flag"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"maps"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"

	"golang.org/x/tools/go/packages"
)

const (
	// modulePrefix bounds the walk to this module's own packages. Dependencies
	// are loaded (NeedDeps is what makes the types resolve) but never scanned.
	modulePrefix = "github.com/aalpar/wile"

	// scopePkgPath and scopeTypeName identify the element type. The scope type
	// lives in pkg/values and is re-exported by pkg/syntax as an alias, so the
	// package path here is the only one that ever matches — see the alias trap.
	scopePkgPath  = "github.com/aalpar/wile/pkg/values"
	scopeTypeName = "Scope"

	// snippetMax truncates the source text carried on a site. Long enough to
	// identify the expression in a failure report, short enough to stay on one
	// terminal line.
	snippetMax = 110
)

// Site is one operation: the AST node that performs it, and what it performs.
type Site struct {
	File string `json:"file"`
	Line int    `json:"line"`
	Col  int    `json:"col"`
	Kind string `json:"kind"`
	Pkg  string `json:"pkg"`
	Text string `json:"text"`
}

// Census is one whole-tree measurement.
//
// ExprFiles is the alias-trap detector: files holding any scope-slice-typed
// expression at all, declaration spellings included. It is much larger than the
// set of files with operations, and it collapses to a handful when the walk is
// run without types.Unalias.
type Census struct {
	Sites     []Site   `json:"sites"`
	ExprFiles []string `json:"exprFiles"`
	PkgErrors []string `json:"pkgErrors"`
	Packages  int      `json:"packages"`
	Unalias   bool     `json:"unalias"`
	Tests     bool     `json:"tests"`
}

// FileRow is a manifest row: one file's operation count.
type FileRow struct {
	Path  string         `json:"path"`
	Count int            `json:"count"`
	Test  bool           `json:"test"`
	Kinds map[string]int `json:"kinds"`
}

func main() {
	dir := flag.String("dir", ".", "module directory to load")
	noalias := flag.Bool("noalias", false,
		"skip types.Unalias — the documented trap, kept as an auditable control")
	tests := flag.Bool("tests", true, "load test files")
	asJSON := flag.Bool("json", false, "emit the census as JSON instead of the report")
	checkPath := flag.String("check", "", "compare against this manifest; exit 1 on any increase")
	writePath := flag.String("write", "", "regenerate this manifest from the measurement")
	flag.Parse()

	// -noalias is a control for reading the number, never a mode to gate or to
	// generate in: it undercounts by construction, and both of those uses would
	// fail open. Refuse rather than quietly agree.
	if *noalias && (*checkPath != "" || *writePath != "") {
		fmt.Fprintln(os.Stderr,
			"scopeopslint: -noalias undercounts by design and cannot be combined with -check or -write")
		os.Exit(2)
	}
	if !*tests && (*checkPath != "" || *writePath != "") {
		fmt.Fprintln(os.Stderr,
			"scopeopslint: -tests=false omits the test rows the manifest grants; not valid with -check or -write")
		os.Exit(2)
	}

	census, err := load(*dir, !*noalias, *tests)
	if err != nil {
		fmt.Fprintln(os.Stderr, "scopeopslint:", err)
		os.Exit(1)
	}
	if len(census.PkgErrors) > 0 {
		for _, e := range census.PkgErrors {
			fmt.Fprintln(os.Stderr, "scopeopslint: pkgerr:", e)
		}
		fmt.Fprintf(os.Stderr,
			"scopeopslint: %d package error(s) — refusing to report a count from a tree that does not typecheck\n",
			len(census.PkgErrors))
		os.Exit(1)
	}
	rows := fileRows(census.Sites)

	if *writePath != "" {
		writeErr := os.WriteFile(*writePath, []byte(renderManifest(rows, census)), 0o644)
		if writeErr != nil {
			fmt.Fprintln(os.Stderr, "scopeopslint:", writeErr)
			os.Exit(1)
		}
		nt, tt := split(rows)
		fmt.Printf("wrote %s: %d row(s), %d operation(s) (%d non-test, %d test)\n",
			*writePath, len(rows), nt+tt, nt, tt)
		return
	}
	if *checkPath != "" {
		ok := check(*checkPath, rows, census)
		if !ok {
			os.Exit(1)
		}
		return
	}
	if *asJSON {
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", " ")
		_ = enc.Encode(census)
		return
	}
	report(rows, census)
}

// load walks the module at dir and returns the census.
func load(dir string, unalias, tests bool) (Census, error) {
	q := Census{Unalias: unalias, Tests: tests}
	root, err := filepath.Abs(dir)
	if err != nil {
		return q, err
	}
	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedFiles | packages.NeedCompiledGoFiles |
			packages.NeedImports | packages.NeedDeps | packages.NeedTypes |
			packages.NeedSyntax | packages.NeedTypesInfo,
		Dir:   root,
		Tests: tests,
		Env:   append(os.Environ(), "GOWORK=off"),
	}
	pkgs, err := packages.Load(cfg, "./...")
	if err != nil {
		return q, err
	}
	p := &scanner{root: root, unalias: unalias, seen: map[string]Site{}, exprs: map[string]bool{}}
	packages.Visit(pkgs, nil, func(pkg *packages.Package) {
		q.Packages++
		for _, e := range pkg.Errors {
			q.PkgErrors = append(q.PkgErrors, pkg.ID+": "+e.Error())
		}
		p.scanPackage(pkg)
	})
	q.Sites = p.sites()
	q.ExprFiles = slices.Sorted(maps.Keys(p.exprs))
	return q, nil
}

// scanner accumulates sites across packages. The same file is compiled into
// several packages when Tests is set (the package, its internal test variant,
// and the external one), so a site is keyed by absolute position plus kind and
// deduplicated globally rather than per package.
type scanner struct {
	root    string
	unalias bool
	seen    map[string]Site
	exprs   map[string]bool
}

func (p *scanner) sites() []Site {
	q := slices.Collect(maps.Values(p.seen))
	slices.SortFunc(q, func(a, b Site) int {
		return cmp.Or(
			cmp.Compare(a.File, b.File),
			cmp.Compare(a.Line, b.Line),
			cmp.Compare(a.Col, b.Col),
			cmp.Compare(a.Kind, b.Kind),
		)
	})
	return q
}

// ua applies types.Unalias unless the control flag turned it off.
func (p *scanner) ua(t types.Type) types.Type {
	if p.unalias {
		return types.Unalias(t)
	}
	return t
}

// isScopeSlice reports whether t is []*values.Scope, or a defined or aliased
// type whose underlying type is that.
func (p *scanner) isScopeSlice(t types.Type) bool {
	if t == nil {
		return false
	}
	t = p.ua(t)
	s, ok := t.(*types.Slice)
	if !ok {
		// A defined type over the slice, e.g. the pre-flip `type Scopes []*Scope`.
		n, named := t.(*types.Named)
		if !named {
			return false
		}
		s, ok = p.ua(n.Underlying()).(*types.Slice)
	}
	if !ok || s == nil {
		return false
	}
	ptr, ok := p.ua(s.Elem()).(*types.Pointer)
	if !ok {
		return false
	}
	n, ok := p.ua(ptr.Elem()).(*types.Named)
	if !ok {
		return false
	}
	obj := n.Obj()
	if obj == nil || obj.Pkg() == nil {
		return false
	}
	return obj.Name() == scopeTypeName && obj.Pkg().Path() == scopePkgPath
}

func (p *scanner) scanPackage(pkg *packages.Package) {
	if pkg.TypesInfo == nil {
		return
	}
	if !strings.HasPrefix(pkg.PkgPath, modulePrefix) {
		return
	}
	for _, f := range pkg.Syntax {
		p.scanFile(pkg, f)
	}
}

func (p *scanner) scanFile(pkg *packages.Package, f *ast.File) {
	parent := parents(f)
	ast.Inspect(f, func(n ast.Node) bool {
		e, isExpr := n.(ast.Expr)
		if !isExpr {
			return true
		}
		tv, typed := pkg.TypesInfo.Types[e]
		if !typed || !p.isScopeSlice(tv.Type) {
			return true
		}
		p.exprs[pkg.Fset.Position(e.Pos()).Filename] = true
		// A composite literal is itself a construction AND can be the operand
		// of another operation (`range []*Scope{a, b}`): two distinct AST
		// nodes, so two sites.
		cl, isLit := e.(*ast.CompositeLit)
		if isLit {
			p.record(pkg, "composite-lit", cl)
		}
		kind, at := p.classify(pkg, e, tv, parent[n])
		if kind != "" && kind != "composite-lit" {
			p.record(pkg, kind, at)
		}
		return true
	})
}

func (p *scanner) record(pkg *packages.Package, kind string, at ast.Node) {
	pos := pkg.Fset.Position(at.Pos())
	key := fmt.Sprintf("%s:%d:%d|%s", pos.Filename, pos.Line, pos.Column, kind)
	_, dup := p.seen[key]
	if dup {
		return
	}
	rel, err := filepath.Rel(p.root, pos.Filename)
	if err != nil {
		rel = pos.Filename
	}
	p.seen[key] = Site{
		File: filepath.ToSlash(rel),
		Line: pos.Line,
		Col:  pos.Column,
		Kind: kind,
		Pkg:  trimTestVariant(pkg.PkgPath),
		Text: snippet(pkg.Fset, at),
	}
}

// classify answers: what does par do to e? It returns "" when e is merely being
// read, passed or stored, which is not an operation on the representation.
func (p *scanner) classify(pkg *packages.Package, e ast.Expr, tv types.TypeAndValue, par ast.Node) (string, ast.Node) {
	if tv.IsType() {
		return p.classifyTypeExpr(pkg, e, par)
	}
	if par == nil {
		return "", nil
	}
	switch q := par.(type) {
	case *ast.CallExpr:
		return p.classifyCall(pkg, e, q)
	case *ast.IndexExpr:
		if q.X == e {
			return "index", q
		}
	case *ast.SliceExpr:
		if q.X == e {
			return "slice-expr", q
		}
	case *ast.RangeStmt:
		if q.X == e {
			return "range", q
		}
	case *ast.BinaryExpr:
		return p.classifyCompare(pkg, e, q)
	}
	return "", nil
}

// classifyTypeExpr handles an expression that IS a type. A type spelling is a
// declaration, not an operation — unless it is the type argument of make or new,
// or the callee of a conversion.
func (p *scanner) classifyTypeExpr(pkg *packages.Package, e ast.Expr, par ast.Node) (string, ast.Node) {
	call, isCall := par.(*ast.CallExpr)
	if !isCall {
		return "", nil
	}
	if len(call.Args) == 0 || call.Args[0] != e {
		// The conversion []*Scope(x) puts the type in Fun, not Args.
		if call.Fun == e {
			return "conversion", call
		}
		return "", nil
	}
	b := builtinName(pkg, call.Fun)
	if b == "make" || b == "new" {
		return "builtin:" + b, call
	}
	if call.Fun == e {
		return "conversion", call
	}
	return "", nil
}

func (p *scanner) classifyCall(pkg *packages.Package, e ast.Expr, call *ast.CallExpr) (string, ast.Node) {
	b := builtinName(pkg, call.Fun)
	if b != "" {
		switch b {
		case "len", "cap", "append", "copy", "clear", "make", "new", "delete":
			if call.Fun != e {
				return "builtin:" + b, call
			}
		}
		return "", nil
	}
	tvf, known := pkg.TypesInfo.Types[call.Fun]
	if known && tvf.IsType() && call.Fun != e {
		return "conversion", call
	}
	// A generic helper that operates ON the representation. The whole point of
	// the opaque type is that these cannot be reached through it.
	name := calleeName(pkg, call.Fun)
	if strings.HasPrefix(name, "slices.") || strings.HasPrefix(name, "sort.") ||
		strings.HasPrefix(name, "reflect.") {
		return name, call
	}
	return "", nil
}

func (p *scanner) classifyCompare(pkg *packages.Package, e ast.Expr, bin *ast.BinaryExpr) (string, ast.Node) {
	if bin.Op != token.EQL && bin.Op != token.NEQ {
		return "", nil
	}
	other := bin.Y
	if bin.Y == e {
		other = bin.X
	}
	if isNilIdent(pkg, other) {
		return "nil-compare", bin
	}
	return "", nil
}

// parents maps every node in f to its parent, so a scope-slice-typed expression
// can be classified by what encloses it.
func parents(f *ast.File) map[ast.Node]ast.Node {
	q := map[ast.Node]ast.Node{}
	var stack []ast.Node
	ast.Inspect(f, func(n ast.Node) bool {
		if n == nil {
			stack = stack[:len(stack)-1]
			return false
		}
		if len(stack) > 0 {
			q[n] = stack[len(stack)-1]
		}
		stack = append(stack, n)
		return true
	})
	return q
}

func isNilIdent(pkg *packages.Package, e ast.Expr) bool {
	id, isIdent := e.(*ast.Ident)
	if !isIdent || id.Name != "nil" {
		return false
	}
	o := pkg.TypesInfo.ObjectOf(id)
	if o == nil {
		return true
	}
	_, isNil := o.(*types.Nil)
	return isNil
}

func builtinName(pkg *packages.Package, fun ast.Expr) string {
	id, isIdent := fun.(*ast.Ident)
	if !isIdent {
		return ""
	}
	b, isBuiltin := pkg.TypesInfo.ObjectOf(id).(*types.Builtin)
	if !isBuiltin {
		return ""
	}
	return b.Name()
}

// calleeName renders a call's callee as "pkg.Func", stripping any explicit
// instantiation (slices.Contains[[]*Scope, *Scope]).
func calleeName(pkg *packages.Package, fun ast.Expr) string {
	for {
		switch f := fun.(type) {
		case *ast.IndexExpr:
			fun = f.X
			continue
		case *ast.IndexListExpr:
			fun = f.X
			continue
		}
		break
	}
	sel, isSel := fun.(*ast.SelectorExpr)
	if !isSel {
		return ""
	}
	fn, isFunc := pkg.TypesInfo.ObjectOf(sel.Sel).(*types.Func)
	if !isFunc || fn.Pkg() == nil {
		return ""
	}
	return fn.Pkg().Name() + "." + fn.Name()
}

// trimTestVariant strips the " [pkg.test]" suffix and the module prefix, so the
// three compilations of one file report one package.
func trimTestVariant(path string) string {
	i := strings.Index(path, " [")
	if i >= 0 {
		path = path[:i]
	}
	path = strings.TrimSuffix(path, ".test")
	// The external test variant's path ends "_test"; report it under the
	// package it tests rather than as a package of its own.
	path = strings.TrimSuffix(path, "_test")
	return strings.TrimPrefix(path, modulePrefix+"/")
}

func snippet(fset *token.FileSet, n ast.Node) string {
	a := fset.Position(n.Pos())
	b := fset.Position(n.End())
	if a.Filename != b.Filename {
		return ""
	}
	data, err := os.ReadFile(a.Filename)
	if err != nil {
		return ""
	}
	if a.Offset < 0 || b.Offset > len(data) || b.Offset < a.Offset {
		return ""
	}
	q := strings.Join(strings.Fields(string(data[a.Offset:b.Offset])), " ")
	if len(q) > snippetMax {
		q = q[:snippetMax] + "..."
	}
	return q
}

// fileRows aggregates sites per file, sorted by path.
func fileRows(sites []Site) []FileRow {
	byFile := map[string]*FileRow{}
	for _, s := range sites {
		r, known := byFile[s.File]
		if !known {
			r = &FileRow{Path: s.File, Test: strings.HasSuffix(s.File, "_test.go"), Kinds: map[string]int{}}
			byFile[s.File] = r
		}
		r.Count++
		r.Kinds[s.Kind]++
	}
	q := make([]FileRow, 0, len(byFile))
	for _, path := range slices.Sorted(maps.Keys(byFile)) {
		q = append(q, *byFile[path])
	}
	return q
}

// split returns the non-test and test operation totals.
func split(rows []FileRow) (nonTest, test int) {
	for _, r := range rows {
		if r.Test {
			test += r.Count
			continue
		}
		nonTest += r.Count
	}
	return nonTest, test
}

// manifestRow matches one rendered row. Paths carry no quotes or backslashes in
// practice; the escape alternation is there so one that did could not end its
// token early.
var manifestRow = regexp.MustCompile(`\("((?:[^"\\]|\\.)*)"\s+(\d+)\)`)

// parseManifest reads the committed per-file grants. Comment lines are dropped
// first so a path quoted inside a comment cannot become a grant.
func parseManifest(text string) map[string]int {
	q := map[string]int{}
	for line := range strings.Lines(text) {
		code := line
		i := strings.Index(code, ";")
		if i >= 0 {
			code = code[:i]
		}
		for _, m := range manifestRow.FindAllStringSubmatch(code, -1) {
			n, err := strconv.Atoi(m[2])
			if err != nil {
				continue
			}
			q[unescape(m[1])] = n
		}
	}
	return q
}

func unescape(s string) string {
	q := strings.ReplaceAll(s, `\"`, `"`)
	return strings.ReplaceAll(q, `\\`, `\`)
}

// check compares the measurement against the manifest and prints what it found.
// It reports true when every file is within its grant.
func check(path string, rows []FileRow, census Census) bool {
	data, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintln(os.Stderr, "scopeopslint:", err)
		return false
	}
	granted := parseManifest(string(data))
	over := make([]FileRow, 0, len(rows))
	nonTestOver := 0
	for _, r := range rows {
		if r.Count <= granted[r.Path] {
			continue
		}
		over = append(over, r)
		if !r.Test {
			nonTestOver++
		}
	}
	if len(over) == 0 {
		nt, tt := split(rows)
		stale := staleRows(granted, rows)
		fmt.Printf("scopeopslint: OK — %d operation(s) (%d non-test, %d test) over %d file(s), all within %s\n",
			nt+tt, nt, tt, len(rows), path)
		if len(stale) > 0 {
			fmt.Printf("  %d manifest row(s) now grant more than is measured; regenerate to ratchet down: %s\n",
				len(stale), strings.Join(stale, " "))
		}
		return true
	}
	fmt.Fprintf(os.Stderr, "scopeopslint: %d file(s) exceed %s\n", len(over), path)
	for _, r := range over {
		fmt.Fprintf(os.Stderr, "  %-58s measured %d  granted %d\n", r.Path, r.Count, granted[r.Path])
		for _, s := range census.Sites {
			if s.File != r.Path {
				continue
			}
			fmt.Fprintf(os.Stderr, "      %s:%d:%d  %-18s %s\n", s.File, s.Line, s.Col, s.Kind, s.Text)
		}
	}
	if nonTestOver > 0 {
		fmt.Fprintf(os.Stderr,
			"\n%d of them are NON-TEST files. A new non-test row is a DEFECT, not drift:\n"+
				"a scope set is values.Scopes, and the slice representation is gone. Fix the site;\n"+
				"do not regenerate the manifest to silence a diff you have not read.\n", nonTestOver)
	}
	return false
}

// staleRows lists manifest paths whose grant now exceeds the measurement.
func staleRows(granted map[string]int, rows []FileRow) []string {
	measured := make(map[string]int, len(rows))
	for _, r := range rows {
		measured[r.Path] = r.Count
	}
	q := make([]string, 0, len(granted))
	for _, path := range slices.Sorted(maps.Keys(granted)) {
		if granted[path] > measured[path] {
			q = append(q, path)
		}
	}
	return q
}

// renderManifest writes the rows as an S-expression list of (path count) pairs,
// one per line, behind a header that states what the numbers mean.
func renderManifest(rows []FileRow, census Census) string {
	nt, tt := split(rows)
	var b strings.Builder
	b.WriteString(manifestHeader)
	fmt.Fprintf(&b, ";; Measured at generation: %d operation(s) — %d non-test, %d test — over %d\n",
		nt+tt, nt, tt, len(rows))
	fmt.Fprintf(&b, ";; file(s), out of %d file(s) holding any scope-slice-typed expression.\n",
		len(census.ExprFiles))
	b.WriteString(";;\n")
	if len(rows) == 0 {
		b.WriteString("()\n")
		return b.String()
	}
	b.WriteByte('(')
	for i, r := range rows {
		if i > 0 {
			b.WriteString("\n ")
		}
		b.WriteByte('(')
		writeSchemeString(&b, r.Path)
		fmt.Fprintf(&b, " %d)", r.Count)
	}
	b.WriteString(")\n")
	return b.String()
}

func writeSchemeString(b *strings.Builder, s string) {
	b.WriteByte('"')
	for _, r := range s {
		if r == '"' || r == '\\' {
			b.WriteByte('\\')
		}
		b.WriteRune(r)
	}
	b.WriteByte('"')
}

// report prints the human census: totals, the alias-trap control, per package,
// per kind, per file, and every site.
func report(rows []FileRow, census Census) {
	nt, tt := split(rows)
	fmt.Printf("TOTAL %d  (non-test %d | test %d)\n", nt+tt, nt, tt)
	ntf, tf := 0, 0
	for _, f := range census.ExprFiles {
		if strings.HasSuffix(f, "_test.go") {
			tf++
			continue
		}
		ntf++
	}
	fmt.Printf("FILES with any scope-slice-typed expression: %d (non-test %d | test %d)  [unalias=%v tests=%v]\n",
		len(census.ExprFiles), ntf, tf, census.Unalias, census.Tests)
	fmt.Printf("FILES with >=1 operation: %d   (packages visited: %d)\n", len(rows), census.Packages)

	byPkg := map[string][2]int{}
	byKind := map[string][3]int{}
	for _, s := range census.Sites {
		isTest := strings.HasSuffix(s.File, "_test.go")
		slot := 0
		if isTest {
			slot = 1
		}
		v := byPkg[s.Pkg]
		v[slot]++
		byPkg[s.Pkg] = v
		k := byKind[s.Kind]
		k[0]++
		k[1+slot]++
		byKind[s.Kind] = k
	}

	fmt.Println("\n--- per package (total | non-test | test)")
	for _, k := range sortedByTotal(byPkg) {
		v := byPkg[k]
		fmt.Printf("%-28s %4d  %4d  %4d\n", k, v[0]+v[1], v[0], v[1])
	}
	fmt.Println("\n--- per kind (total | non-test | test)")
	kinds := slices.SortedFunc(maps.Keys(byKind), func(a, b string) int {
		return cmp.Or(cmp.Compare(byKind[b][0], byKind[a][0]), cmp.Compare(a, b))
	})
	for _, k := range kinds {
		v := byKind[k]
		fmt.Printf("%-22s %4d  %4d  %4d\n", k, v[0], v[1], v[2])
	}
	fmt.Println("\n--- per file")
	for _, r := range rows {
		fmt.Printf("%5d  %s\n", r.Count, r.Path)
	}
	fmt.Println("\n--- all sites")
	for _, s := range census.Sites {
		fmt.Printf("%s:%d:%d\t%s\t%s\n", s.File, s.Line, s.Col, s.Kind, s.Text)
	}
}

func sortedByTotal(m map[string][2]int) []string {
	return slices.SortedFunc(maps.Keys(m), func(a, b string) int {
		ta := m[a][0] + m[a][1]
		tb := m[b][0] + m[b][1]
		return cmp.Or(cmp.Compare(tb, ta), cmp.Compare(a, b))
	})
}

// manifestHeader is the comment block every generated manifest carries. It is
// here rather than in the manifest so a regeneration cannot quietly drop it.
const manifestHeader = `;; scope-ops manifest — raw slice operations on a hygiene scope set.
;;
;; GENERATED by tools/cmd/scopeopslint; asserted by TestScopeOpsManifest in
;; pkg/values. Regenerate with:
;;
;;   WILE_SCOPE_OPS_UPDATE=1 go test -count=1 -run TestScopeOpsManifest ./pkg/values/
;;
;; Baseline. At the pre-flip commit d2927bea the same walk read 423 operations —
;; 94 non-test, 329 test — over 49 files, out of 91 files holding any
;; scope-slice-typed expression. A scope set was []*syntax.Scope / []*values.Scope
;; then; it is now the opaque values.Scopes persistent chain, which exposes no
;; Slice() and no indexed access because member order is an implementation
;; invariant rather than part of the contract. Run with -noalias to see what the
;; alias trap measures instead: 118 operations over 11 files at that same commit,
;; out of 13 files holding any scope-slice-typed expression rather than 91,
;; because syntax.Scope is a type ALIAS of values.Scope and without types.Unalias
;; the walk sees only pkg/values.
;;
;; What is measured. Every expression go/types types as []*values.Scope, after
;; types.Unalias, classified by what its PARENT node does to it: len, cap, range,
;; index, slice expression, append, copy, clear, make, new, conversion, composite
;; literal (EMPTY ONES INCLUDED), comparison against nil, or use as an argument to
;; a slices.*, sort.* or reflect.* helper. Reading, passing or storing the value is
;; not an operation.
;;
;; How to read a diff. Each row is a CEILING, not an equality: (path count) grants
;; that file at most count operations, and a file with no row is granted zero.
;; Removing operations passes, and the manifest is then ratcheted down by
;; regenerating. A row APPEARING is a defect rather than drift — the representation
;; it operates on no longer exists as a contract, so a new site is code that
;; reintroduced it. NEVER regenerate to silence a diff you have not read.
;;
;; The two non-test rows are the boundary, and they are the only ones permitted:
;; ScopesFromSlice is the declared slice→Scopes constructor, so it necessarily
;; ranges its input, and SourceContext.WithScope hands it a one-element literal on
;; the nil-receiver path. TestScopeOpsManifest names both and rejects any third.
;;
`
