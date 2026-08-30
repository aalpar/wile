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

// Command deadscan reports exported symbols that no production code consumes.
//
// It is a REPORTER, not a gate: it exits 0 on a successful scan and is not
// wired into `make lint`. It exists because `deadcode` does not run on this
// toolchain and `unused` skips exported identifiers outright, so the exported
// surface is the one place nothing measures.
//
// # What "dead" means here
//
// A symbol is dead when no file classified as production references it, where
// production is everything under the wile module that is not a _test.go file
// and not one of the test-support trees (integration/, tools/, pkg/testutil,
// the five *test helper packages). References from a sibling workspace module
// count as live: wile-goast builds against this API, so its calls are real
// consumers.
//
// Those out-of-module references are reported per consuming module, because the
// two kinds do not mean the same thing. Both keep a symbol LIVE — deleting it
// breaks that module's build either way. What they answer differently is
// whether the symbol is public API, and only an INDEPENDENT consumer is
// evidence that it is. On this tree wile-goast accounts for 95 of the 102, and
// wile-goast is first-party: it is the analyzer Wile runs on its own source, so
// a reference from it says we use our own internals, not that an embedder
// depends on them. wile-extension-example, at 37, is the independent proxy.
//
// Two refinements are what separate this from grep. References from inside the
// symbol's own body — and, for a type, from inside its own methods — do not
// count, or every self-recursive function scores live. And a concrete method
// counts references to the INTERFACE method it satisfies, so the ~130
// implementations of values.Value.EqualTo are correctly live despite having no
// direct caller.
//
// # Pins: the reasons a reference count is the wrong question
//
// Four classes of symbol cannot be deleted no matter what the count says. Each
// is recorded as a pin, and a pinned symbol is a live ROOT, so liveness
// propagates out of it:
//
//   - assert — a compile-time satisfaction assertion, `var _ I = (*T)(nil)`,
//     pins every method I requires of T. Deleting one is a compile error, and
//     the assertion names the type, never the methods, so nothing in a
//     reference count can see it.
//   - external-iface — the receiver satisfies an interface owned outside this
//     module (readline.AutoCompleter, quicktest.Checker, driver.Validator).
//     The caller is in someone else's code.
//   - error-protocol — Error/Unwrap/Is/As on a type implementing the universe
//     `error`. `error` lives in no package scope, so interface mediation cannot
//     find it by walking packages.
//   - anon-iface — reached through an anonymous interface assertion such as
//     node.(interface{ AddScope(*Scope) SyntaxValue }). Deleting the method
//     does not break the build; the assertion just stops matching, silently.
//
// Making pins live roots is load-bearing beyond the pinned symbol itself.
// BindingRefInvalid is read on every BindingRef.IsValid call and IsValid is
// external-iface pinned; without root treatment the fixpoint reached past
// IsValid and reported a constant that is read on every call as dead.
//
// # Two further reasons a row is not a work order
//
// iotagroup records that a constant belongs to an iota-driven block. Removing
// one member renumbers every name after it, so it is never an independent
// deletion however dead it is.
//
// clusterwith names the dead symbols that reference this one. A non-empty list
// means the row cannot be removed alone — VerifyExpanders is genuinely
// production-dead, but only reachable from VerifyAllPhaseHandlers, and deleting
// the leaf without its caller is not a change anyone would make.
//
// # Known limits
//
// Dominance-unreachable code is invisible: the reference count is nonzero and
// the execution count is zero. That needs CFG reachability per call site.
//
// Unexported symbols are out of scope — the filter is ast.IsExported, so
// deadcode's main yield is absent and the true dead total is larger.
//
// Reflection and serialization are invisible: encoding/json reads struct fields
// with no identifier reference, so a JSON-tagged payload struct reads as
// write-only.
//
// A comparable struct used as a map key or compared with == reads every field
// with no selector, so its fields read as write-only (environment.BindingID).
//
// Only the host build configuration is type-checked: files behind another
// GOOS/GOARCH tag, and go:linkname consumers, are not seen.
//
// Finally, a Go symbol reached only from .scm source is invisible — though so
// is the reverse, and no primitive body appears here, since those are all
// reachable through their registry specs.
//
// # Usage
//
//	go run ./tools/cmd/deadscan ./...
//	go run ./tools/cmd/deadscan -json ./... > syms.json
//	make -C tools deadscan
//
// Pass every workspace module whose consumers should count, or the ext column
// is zero and the dead list is overstated. The report says how many modules it
// saw so that mistake is loud rather than silent.
package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"

	"golang.org/x/tools/go/packages"
)

// wileModule is the module whose exported surface is under audit. Packages from
// any other module are consumers, and a reference from one is a live use.
const wileModule = "github.com/aalpar/wile"

// auditedTrees are the module-relative directory prefixes whose exported
// symbols are audited. Everything else in the module is either test support or
// not part of the surface a consumer can reach.
var auditedTrees = []string{"pkg/", "extensions/", "cmd/"}

// testTrees are the module-relative prefixes whose files are test support even
// though they are not _test.go. A reference from one of these does not keep a
// symbol alive: being production-dead is the contract of a test helper.
var testTrees = []string{
	"integration/", "test/", "tools/", "experiments/", "scripts/", "benchmarks/",
	"pkg/testutil/", "pkg/registry/testhelpers/", "pkg/syntax/syntaxtest/",
	"pkg/values/valuestest/", "pkg/internal/extensions/iotest/",
}

// pinRank orders the pin reasons so a symbol carrying more than one reports the
// most specific. assert outranks the rest because it is a compile error rather
// than a behaviour change.
var pinRank = map[string]int{"assert": 4, "external-iface": 3, "error-protocol": 2, "anon-iface": 1}

// symbol is one exported declaration and everything measured about it.
type symbol struct {
	Key     string `json:"key"`
	Name    string `json:"name"`
	Kind    string `json:"kind"`
	Recv    string `json:"recv,omitempty"`
	PkgPath string `json:"pkg"`
	Pos     string `json:"pos"`
	LOC     int    `json:"loc"`

	// ProdRefs is self- and cluster-filtered: a reference from the symbol's own
	// body, or from a sibling method of its own type, is not a use.
	ProdRefs int `json:"prod"`
	TestRefs int `json:"test"`
	// ExtRefs counts references from another workspace module — a real
	// out-of-module consumer, and the strongest liveness signal there is.
	ExtRefs int `json:"ext"`
	// ExtBy splits ExtRefs by the consuming module, because the two kinds of
	// consumer do not mean the same thing. Both keep the symbol LIVE — deleting
	// it breaks that module's build either way, and nothing here changes the
	// dead/alive verdict. What they answer differently is whether the symbol is
	// public API: a reference from an independent embedder is evidence that it
	// is, and a reference from first-party tooling that happens to live in
	// another module is only evidence that we use our own internals.
	ExtBy map[string]int `json:"extby,omitempty"`
	// Reads and Writes are meaningful for fields only. Writes >> Reads == 0 is
	// a field nothing consults.
	Reads  int `json:"reads,omitempty"`
	Writes int `json:"writes,omitempty"`

	// IfaceIn names the in-tree interface methods this method satisfies, and
	// IfaceRefs counts production calls through them.
	IfaceIn   []string `json:"ifacein,omitempty"`
	IfaceRefs int      `json:"ifacerefs,omitempty"`
	AnonIface int      `json:"anoniface,omitempty"`

	// Pin, when set, names why this symbol cannot be deleted regardless of its
	// counts. See the package comment.
	Pin string `json:"pin,omitempty"`
	// IotaGroup is the number of names in this constant's iota-driven block.
	// Above 1 the member is not independently deletable.
	IotaGroup int `json:"iotagroup,omitempty"`
	// ClusterWith names the dead symbols that reference this one. Non-empty
	// means the row is not a standalone deletion.
	ClusterWith []string `json:"clusterwith,omitempty"`

	ProdSite []string `json:"prodsites,omitempty"`
	TestSite []string `json:"testsites,omitempty"`

	Live bool `json:"live"`
}

// pin records a reason this symbol cannot be deleted, keeping the most specific
// of several.
func (p *symbol) pin(reason string) {
	if pinRank[reason] > pinRank[p.Pin] {
		p.Pin = reason
	}
}

// standalone reports whether the symbol can be removed on its own: dead, not a
// member of an iota block, and not reachable from another dead symbol.
func (p *symbol) standalone() bool {
	return !p.Live && p.IotaGroup < 2 && len(p.ClusterWith) == 0
}

// declRange is one top-level declaration's source extent, used to attribute a
// reference to the declaration that encloses it.
type declRange struct {
	start, end token.Pos
	key        string
	cluster    string
}

// scanner holds one scan's accumulated state. It is a struct rather than a set
// of package globals so a test can run several scans in one process.
type scanner struct {
	syms      map[string]*symbol
	seenDecl  map[string]bool
	seenRef   map[string]bool
	fieldKey  map[token.Pos]string
	namedByPk map[string]*types.Named
	ifaceDecl map[string][]*types.Named

	ifaceMethodRefs map[string]int
	anonIfaceRefs   map[string]int

	// refFrom maps a symbol key to the enclosing declarations that reference it
	// from production code. "<file>" stands for a reference at file scope.
	refFrom map[string]map[string]bool
	// declRanges maps an absolute filename to its top-level declaration extents.
	declRanges map[string][]declRange
	// modules records every module path seen, so the report can say whether the
	// consumer side was actually loaded.
	modules map[string]bool
}

func newScanner() *scanner {
	return &scanner{
		syms:            map[string]*symbol{},
		seenDecl:        map[string]bool{},
		seenRef:         map[string]bool{},
		fieldKey:        map[token.Pos]string{},
		namedByPk:       map[string]*types.Named{},
		ifaceDecl:       map[string][]*types.Named{},
		ifaceMethodRefs: map[string]int{},
		anonIfaceRefs:   map[string]int{},
		refFrom:         map[string]map[string]bool{},
		declRanges:      map[string][]declRange{},
		modules:         map[string]bool{},
	}
}

func main() {
	asJSON := flag.Bool("json", false, "emit every scanned symbol as JSON instead of the report")
	dir := flag.String("dir", ".", "directory to load packages from")
	flag.Parse()
	patterns := flag.Args()
	if len(patterns) == 0 {
		patterns = []string{"./..."}
	}

	p := newScanner()
	err := p.load(*dir, patterns)
	if err != nil {
		fmt.Fprintln(os.Stderr, "deadscan:", err)
		os.Exit(1)
	}
	if *asJSON {
		p.writeJSON()
		return
	}
	p.report()
}

// load type-checks the patterns and runs every pass in order. The order is not
// arbitrary: declarations must exist before references can be attributed to
// them, and every pin must be set before the fixpoint treats pins as roots.
func (p *scanner) load(dir string, patterns []string) error {
	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedFiles | packages.NeedCompiledGoFiles |
			packages.NeedImports | packages.NeedDeps | packages.NeedTypes |
			packages.NeedSyntax | packages.NeedTypesInfo | packages.NeedModule,
		Dir:   dir,
		Tests: true,
	}
	pkgs, err := packages.Load(cfg, patterns...)
	if err != nil {
		return err
	}
	all := map[string]*packages.Package{}
	packages.Visit(pkgs, nil, func(q *packages.Package) {
		if q.Types != nil && len(q.Syntax) > 0 {
			all[q.ID] = q
		}
	})
	keys := make([]string, 0, len(all))
	for k := range all {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	p.run(all, keys)
	return nil
}

// run is load's second half, split out so a test can drive the passes over
// hand-built packages without a filesystem.
func (p *scanner) run(all map[string]*packages.Package, keys []string) {
	for _, k := range keys {
		p.collectIfaces(all[k])
	}
	for _, k := range keys {
		p.collectDecls(all[k])
	}
	for _, k := range keys {
		p.collectRefs(all[k])
	}
	for _, k := range keys {
		p.collectAssertions(all[k])
	}
	for _, s := range p.syms {
		if s.Kind == "method" {
			s.AnonIface = p.anonIfaceRefs[s.Name]
		}
	}
	p.mediateInterfaces(all, keys)
	p.pinErrorProtocol()
	p.fixpoint()
	p.computeClusters()
}

// ---- classification ----

// moduleDir returns the package's module root, or "" when the package has no
// module (the standard library).
func moduleDir(p *packages.Package) string {
	if p.Module == nil {
		return ""
	}
	return p.Module.Dir
}

// relTo makes an absolute filename module-relative, so a citation reads
// pkg/machine/peephole.go:282 rather than an absolute path that depends on
// where the tree happens to be checked out.
func relTo(root, file string) string {
	if root == "" {
		return file
	}
	rel, err := filepath.Rel(root, file)
	if err != nil {
		return file
	}
	return filepath.ToSlash(rel)
}

// classify sorts a file into prod, test, or ext.
//
// It reads the file's position relative to its own MODULE root rather than
// searching the absolute path for a directory name. An earlier form matched the
// literal string "wile-workspace/", which silently classified every file as ext
// — and so every symbol as live — in a checkout under any other name.
func classify(p *packages.Package, file string) string {
	if strings.HasSuffix(file, "_test.go") {
		return "test"
	}
	if p.Module == nil || p.Module.Path != wileModule {
		return "ext"
	}
	rel := relTo(p.Module.Dir, file)
	for _, t := range testTrees {
		if strings.HasPrefix(rel, t) {
			return "test"
		}
	}
	return "prod"
}

// inScope reports whether a package's exported symbols are audited at all.
func inScope(pkgPath string) bool {
	if !strings.HasPrefix(pkgPath, wileModule) {
		return false
	}
	rest := strings.TrimPrefix(strings.TrimPrefix(pkgPath, wileModule), "/")
	for _, t := range auditedTrees {
		if strings.HasPrefix(rest, t) {
			return true
		}
	}
	return false
}

func recvName(t types.Type) string {
	n, ok := deref(t).(*types.Named)
	if ok {
		return n.Obj().Name()
	}
	return types.TypeString(t, nil)
}

func deref(t types.Type) types.Type {
	p, ok := t.(*types.Pointer)
	if ok {
		return p.Elem()
	}
	return t
}

// objKey is a symbol's stable identity: package path, receiver when it has one,
// and name. Fields are keyed separately, via fieldKey, because a types.Var for
// a field carries no receiver.
func objKey(obj types.Object) string {
	if obj == nil || obj.Pkg() == nil {
		return ""
	}
	switch o := obj.(type) {
	case *types.Func:
		sig, _ := o.Type().(*types.Signature)
		if sig != nil && sig.Recv() != nil {
			return o.Pkg().Path() + "." + recvName(sig.Recv().Type()) + "." + o.Name()
		}
		return o.Pkg().Path() + "." + o.Name()
	case *types.Var:
		if o.IsField() {
			return ""
		}
		return o.Pkg().Path() + "." + o.Name()
	case *types.TypeName, *types.Const:
		return obj.Pkg().Path() + "." + obj.Name()
	}
	return ""
}

// clusterOf names the declaration group a symbol belongs to: its receiver type
// for a method or field, itself otherwise. A method referencing a sibling
// method of the same type is not a use.
func clusterOf(pkgPath, kind, recv, name string) string {
	if kind == "method" || kind == "field" {
		return pkgPath + "." + recv
	}
	return pkgPath + "." + name
}

// ---- interfaces ----

// collectIfaces indexes every in-tree interface by the method names it
// declares, so a concrete method can later find the interfaces it satisfies.
func (p *scanner) collectIfaces(pkg *packages.Package) {
	scope := pkg.Types.Scope()
	for _, nm := range scope.Names() {
		tn, ok := scope.Lookup(nm).(*types.TypeName)
		if !ok {
			continue
		}
		n, ok := tn.Type().(*types.Named)
		if !ok {
			continue
		}
		it, ok := n.Underlying().(*types.Interface)
		if !ok {
			continue
		}
		for m := range it.Methods() {
			p.ifaceDecl[m.Name()] = appendUniqueNamed(p.ifaceDecl[m.Name()], n)
		}
	}
}

func appendUniqueNamed(xs []*types.Named, n *types.Named) []*types.Named {
	for _, x := range xs {
		if x.Obj() == n.Obj() {
			return xs
		}
	}
	return append(xs, n)
}

// mediateInterfaces attributes interface-level calls to the concrete methods
// that satisfy them, and pins the two classes a call count cannot decide.
func (p *scanner) mediateInterfaces(all map[string]*packages.Package, keys []string) {
	for _, k := range keys {
		pkg := all[k]
		sc := pkg.Types.Scope()
		for _, nm := range sc.Names() {
			tn, ok := sc.Lookup(nm).(*types.TypeName)
			if !ok {
				continue
			}
			n, ok := tn.Type().(*types.Named)
			if ok {
				p.namedByPk[basePath(pkg)+"."+nm] = n
			}
		}
	}
	for _, s := range p.syms {
		if s.Kind != "method" {
			continue
		}
		p.mediateOne(s)
	}
}

func (p *scanner) mediateOne(s *symbol) {
	n := p.namedByPk[s.PkgPath+"."+s.Recv]
	if n == nil {
		return
	}
	seen := map[string]bool{}
	for _, it := range p.ifaceDecl[s.Name] {
		iface, _ := it.Underlying().(*types.Interface)
		if iface == nil || iface.NumMethods() == 0 {
			continue
		}
		if !implements(n, iface) {
			continue
		}
		ikey := it.Obj().Pkg().Path() + "." + it.Obj().Name() + "." + s.Name
		// Loading with Tests:true yields a second *types.Named for every
		// interface in a package that has an external test package, so without
		// this the same interface is credited twice.
		if seen[ikey] {
			continue
		}
		seen[ikey] = true
		s.IfaceIn = append(s.IfaceIn, ikey)
		s.IfaceRefs += p.ifaceMethodRefs[ikey]
		if !strings.HasPrefix(it.Obj().Pkg().Path(), wileModule) {
			s.pin("external-iface")
		}
	}
	sort.Strings(s.IfaceIn)
	if s.AnonIface > 0 {
		s.pin("anon-iface")
	}
}

func implements(n *types.Named, it *types.Interface) bool {
	return types.Implements(n, it) || types.Implements(types.NewPointer(n), it)
}

// pinErrorProtocol pins the methods dispatched through the universe error
// interface and the errors package's matching protocol. error belongs to no
// package scope, so collectIfaces never sees it and interface mediation scores
// every Error() zero.
func (p *scanner) pinErrorProtocol() {
	errIface, _ := types.Universe.Lookup("error").Type().Underlying().(*types.Interface)
	if errIface == nil {
		return
	}
	for _, s := range p.syms {
		if s.Kind != "method" {
			continue
		}
		switch s.Name {
		case "Error", "Unwrap", "Is", "As":
		default:
			continue
		}
		n := p.namedByPk[s.PkgPath+"."+s.Recv]
		if n != nil && implements(n, errIface) {
			s.pin("error-protocol")
		}
	}
}

// ---- declarations ----

func basePath(p *packages.Package) string {
	return strings.TrimSuffix(p.PkgPath, "_test")
}

func (p *scanner) collectDecls(pkg *packages.Package) {
	// Main is the discriminator that matters: under a go.work every `use`d
	// module is main, and everything else reachable is a dependency in the
	// module cache. Counting all of them would report 26 and make the
	// one-module warning below unreachable.
	if pkg.Module != nil && pkg.Module.Main {
		p.modules[pkg.Module.Path] = true
	}
	base := basePath(pkg)
	if !inScope(base) {
		return
	}
	for _, f := range pkg.Syntax {
		fname := pkg.Fset.Position(f.Pos()).Filename
		if strings.HasSuffix(fname, "_test.go") {
			continue
		}
		for _, d := range f.Decls {
			p.collectDecl(pkg, base, fname, d)
		}
	}
}

func (p *scanner) collectDecl(pkg *packages.Package, base, fname string, d ast.Decl) {
	switch d := d.(type) {
	case *ast.FuncDecl:
		p.collectFuncDecl(pkg, base, fname, d)
	case *ast.GenDecl:
		for _, spec := range d.Specs {
			p.collectSpec(pkg, base, fname, d, spec)
		}
	}
}

func (p *scanner) collectFuncDecl(pkg *packages.Package, base, fname string, d *ast.FuncDecl) {
	obj := pkg.TypesInfo.Defs[d.Name]
	if obj == nil {
		return
	}
	kind, recv := "func", ""
	if d.Recv != nil {
		kind = "method"
		fn, ok := obj.(*types.Func)
		if ok {
			recv = recvName(fn.Type().(*types.Signature).Recv().Type())
		}
	}
	k := objKey(obj)
	p.addRange(fname, d.Pos(), d.End(), k, clusterOf(base, kind, recv, obj.Name()))
	p.addDecl(pkg, k, obj.Name(), kind, recv, d.Pos(), d.End())
}

func (p *scanner) collectSpec(pkg *packages.Package, base, fname string, d *ast.GenDecl, spec ast.Spec) {
	switch s := spec.(type) {
	case *ast.TypeSpec:
		p.collectTypeSpec(pkg, base, fname, s)
	case *ast.ValueSpec:
		p.collectValueSpec(pkg, base, fname, d, s)
	}
}

func (p *scanner) collectTypeSpec(pkg *packages.Package, base, fname string, s *ast.TypeSpec) {
	obj := pkg.TypesInfo.Defs[s.Name]
	if obj == nil {
		return
	}
	k := objKey(obj)
	p.addRange(fname, s.Pos(), s.End(), k, base+"."+s.Name.Name)
	p.addDecl(pkg, k, obj.Name(), "type", "", s.Pos(), s.End())
	st, ok := s.Type.(*ast.StructType)
	if !ok {
		return
	}
	for _, fl := range st.Fields.List {
		p.collectFields(pkg, base, s.Name.Name, fl)
	}
}

func (p *scanner) collectFields(pkg *packages.Package, base, typeName string, fl *ast.Field) {
	for _, nm := range fl.Names {
		fo, _ := pkg.TypesInfo.Defs[nm].(*types.Var)
		if fo == nil {
			continue
		}
		fk := base + "." + typeName + "." + nm.Name
		p.fieldKey[fo.Pos()] = fk
		p.addDecl(pkg, fk, nm.Name, "field", typeName, nm.Pos(), fl.End())
	}
}

func (p *scanner) collectValueSpec(pkg *packages.Package, base, fname string, d *ast.GenDecl, s *ast.ValueSpec) {
	kind := "var"
	if d.Tok == token.CONST {
		kind = "const"
	}
	group := 0
	if kind == "const" && usesIota(d) {
		group = countNames(d)
	}
	for _, nm := range s.Names {
		obj := pkg.TypesInfo.Defs[nm]
		if obj == nil {
			continue
		}
		k := objKey(obj)
		p.addRange(fname, s.Pos(), s.End(), k, base+"."+nm.Name)
		p.addDecl(pkg, k, obj.Name(), kind, "", nm.Pos(), s.End())
		sym := p.syms[k]
		if sym != nil {
			sym.IotaGroup = group
		}
	}
}

// usesIota reports whether any spec in a const block mentions iota, which is
// what makes the block's names positional and its members interdependent.
func usesIota(d *ast.GenDecl) bool {
	found := false
	ast.Inspect(d, func(n ast.Node) bool {
		id, ok := n.(*ast.Ident)
		if ok && id.Name == "iota" {
			found = true
		}
		return !found
	})
	return found
}

// countNames totals the declared names across a GenDecl's value specs.
func countNames(d *ast.GenDecl) int {
	q := 0
	for _, spec := range d.Specs {
		vs, ok := spec.(*ast.ValueSpec)
		if ok {
			q += len(vs.Names)
		}
	}
	return q
}

func (p *scanner) addRange(file string, start, end token.Pos, key, cluster string) {
	if key == "" {
		return
	}
	p.declRanges[file] = append(p.declRanges[file], declRange{start, end, key, cluster})
}

func (p *scanner) addDecl(pkg *packages.Package, key, name, kind, recv string, start, end token.Pos) {
	if key == "" || !ast.IsExported(name) || p.seenDecl[key] {
		return
	}
	p.seenDecl[key] = true
	sl := pkg.Fset.Position(start).Line
	el := pkg.Fset.Position(end).Line
	p.syms[key] = &symbol{
		Key: key, Name: name, Kind: kind, Recv: recv,
		PkgPath: basePath(pkg),
		Pos:     p.posOf(pkg, start),
		LOC:     el - sl + 1,
	}
}

func (p *scanner) posOf(pkg *packages.Package, pos token.Pos) string {
	pp := pkg.Fset.Position(pos)
	return fmt.Sprintf("%s:%d", relTo(moduleDir(pkg), pp.Filename), pp.Line)
}

// ---- references ----

// enclosing returns the innermost top-level declaration containing pos, which
// is the declaration credited with making the reference.
func (p *scanner) enclosing(file string, pos token.Pos) (key, cluster string) {
	rs := p.declRanges[file]
	best := -1
	for i, r := range rs {
		if pos < r.start || pos >= r.end {
			continue
		}
		if best < 0 || (r.end-r.start) < (rs[best].end-rs[best].start) {
			best = i
		}
	}
	if best < 0 {
		return "", ""
	}
	return rs[best].key, rs[best].cluster
}

func (p *scanner) collectRefs(pkg *packages.Package) {
	for _, f := range pkg.Syntax {
		fname := pkg.Fset.Position(f.Pos()).Filename
		cls := classify(pkg, fname)
		parent := map[ast.Node]ast.Node{}
		var stack []ast.Node
		ast.Inspect(f, func(n ast.Node) bool {
			if n == nil {
				if len(stack) > 0 {
					stack = stack[:len(stack)-1]
				}
				return false
			}
			if len(stack) > 0 {
				parent[n] = stack[len(stack)-1]
			}
			stack = append(stack, n)
			id, ok := n.(*ast.Ident)
			if ok {
				p.noteRef(pkg, fname, cls, parent, id)
			}
			return true
		})
	}
}

func (p *scanner) noteRef(pkg *packages.Package, fname, cls string, parent map[ast.Node]ast.Node, id *ast.Ident) {
	obj := pkg.TypesInfo.Uses[id]
	if obj == nil {
		return
	}
	if cls == "prod" {
		p.noteInterfaceRef(pkg, obj, id)
	}
	key := objKey(obj)
	v, isVar := obj.(*types.Var)
	if isVar && v.IsField() {
		key = p.fieldKey[v.Pos()]
	}
	if key == "" {
		return
	}
	s := p.syms[key]
	if s == nil {
		return
	}
	site := p.posOf(pkg, id.Pos())
	if p.seenRef[key+"@"+site] {
		return
	}
	p.seenRef[key+"@"+site] = true
	p.credit(s, key, fname, cls, site, modulePath(pkg), parent, id)
}

// modulePath names the module a package belongs to, or "" for the standard
// library, which has none.
func modulePath(p *packages.Package) string {
	if p.Module == nil {
		return ""
	}
	return p.Module.Path
}

// credit records one reference against a symbol, applying the self/cluster
// filter that keeps a symbol from keeping itself alive.
func (p *scanner) credit(s *symbol, key, fname, cls, site, consumer string, parent map[ast.Node]ast.Node, id *ast.Ident) {
	ek, ecl := "", ""
	if cls == "prod" {
		ek, ecl = p.enclosing(fname, id.Pos())
		if p.refFrom[key] == nil {
			p.refFrom[key] = map[string]bool{}
		}
		if ek != "" {
			p.refFrom[key][ek] = true
		} else {
			p.refFrom[key]["<file>"] = true
		}
	}
	if s.Kind == "field" {
		if isWrite(parent, id) {
			s.Writes++
		} else {
			s.Reads++
		}
	}
	switch cls {
	case "test":
		s.TestRefs++
		if len(s.TestSite) < 4 {
			s.TestSite = append(s.TestSite, site)
		}
	case "ext":
		s.ExtRefs++
		if s.ExtBy == nil {
			s.ExtBy = map[string]int{}
		}
		s.ExtBy[consumer]++
		if len(s.ProdSite) < 8 {
			s.ProdSite = append(s.ProdSite, "EXT "+site)
		}
	default:
		myCluster := clusterOf(s.PkgPath, s.Kind, s.Recv, s.Name)
		if ek == key || (s.Kind == "type" && ecl == myCluster) {
			return
		}
		s.ProdRefs++
		if len(s.ProdSite) < 8 {
			s.ProdSite = append(s.ProdSite, site)
		}
	}
}

// noteInterfaceRef records a production call made through an interface, keyed by
// the interface method so every implementation can claim it.
func (p *scanner) noteInterfaceRef(pkg *packages.Package, obj types.Object, id *ast.Ident) {
	fn, ok := obj.(*types.Func)
	if !ok {
		return
	}
	sig, _ := fn.Type().(*types.Signature)
	if sig == nil || sig.Recv() == nil {
		return
	}
	_, isIface := sig.Recv().Type().Underlying().(*types.Interface)
	if !isIface {
		return
	}
	site := p.posOf(pkg, id.Pos())
	ik := "anon." + fn.Name()
	named, isNamed := sig.Recv().Type().(*types.Named)
	if isNamed && named.Obj().Pkg() != nil {
		ik = named.Obj().Pkg().Path() + "." + named.Obj().Name() + "." + fn.Name()
	}
	if p.seenRef["IFACE"+ik+"@"+site] {
		return
	}
	p.seenRef["IFACE"+ik+"@"+site] = true
	if strings.HasPrefix(ik, "anon.") {
		p.anonIfaceRefs[fn.Name()]++
		return
	}
	p.ifaceMethodRefs[ik]++
}

// isWrite reports whether a selector on a field is the target of an assignment
// or a composite-literal key, rather than a read of it.
func isWrite(parent map[ast.Node]ast.Node, n ast.Node) bool {
	pn := parent[n]
	if pn == nil {
		return false
	}
	kv, ok := pn.(*ast.KeyValueExpr)
	if ok && kv.Key == n {
		_, inLit := parent[pn].(*ast.CompositeLit)
		if inLit {
			return true
		}
	}
	sel, ok := pn.(*ast.SelectorExpr)
	if !ok || sel.Sel != n {
		return false
	}
	as, ok := parent[sel].(*ast.AssignStmt)
	if !ok || as.Tok != token.ASSIGN {
		return false
	}
	return slices.Contains(as.Lhs, ast.Expr(sel))
}

// ---- assertion pins ----

// collectAssertions finds the compile-time interface-satisfaction assertions
//
//	var _ I = (*T)(nil)
//	var _ I = T{}
//
// and pins every method I requires of T. Nothing in a reference count can see
// these: the assertion mentions the type and never the methods, yet deleting
// any one of them is a compile error rather than a behaviour change.
func (p *scanner) collectAssertions(pkg *packages.Package) {
	for _, f := range pkg.Syntax {
		for _, d := range f.Decls {
			gd, ok := d.(*ast.GenDecl)
			if !ok || gd.Tok != token.VAR {
				continue
			}
			for _, spec := range gd.Specs {
				p.pinAsserted(pkg, spec)
			}
		}
	}
}

func (p *scanner) pinAsserted(pkg *packages.Package, spec ast.Spec) {
	vs, ok := spec.(*ast.ValueSpec)
	if !ok || vs.Type == nil || len(vs.Names) != 1 || len(vs.Values) != 1 {
		return
	}
	if vs.Names[0].Name != "_" {
		return
	}
	dt := pkg.TypesInfo.TypeOf(vs.Type)
	ct := pkg.TypesInfo.TypeOf(vs.Values[0])
	if dt == nil || ct == nil {
		return
	}
	it, _ := dt.Underlying().(*types.Interface)
	if it == nil {
		return
	}
	named, ok := deref(ct).(*types.Named)
	if !ok || named.Obj().Pkg() == nil {
		return
	}
	prefix := named.Obj().Pkg().Path() + "." + named.Obj().Name() + "."
	for method := range it.Methods() {
		s := p.syms[prefix+method.Name()]
		if s != nil {
			s.pin("assert")
		}
	}
}

// ---- liveness ----

// fixpoint removes symbols reachable only from other dead symbols, iterating
// until nothing changes.
//
// A pinned symbol is a live ROOT and is never removed, which is load-bearing
// beyond the pinned symbol itself: without it the walk reaches past a
// live-by-design symbol and kills whatever only that symbol references.
func (p *scanner) fixpoint() {
	live := map[string]bool{}
	for k := range p.syms {
		live[k] = true
	}
	for {
		changed := false
		for k, s := range p.syms {
			if !live[k] || p.rooted(s) {
				continue
			}
			if p.reachable(k, s, live) {
				continue
			}
			live[k] = false
			changed = true
		}
		if !changed {
			break
		}
	}
	for k, s := range p.syms {
		s.Live = live[k]
	}
}

// rooted reports whether a symbol is live regardless of what references it.
func (p *scanner) rooted(s *symbol) bool {
	return s.ExtRefs > 0 || s.Pin != "" || s.Name == "Main"
}

// reachable reports whether any live production referrer keeps s alive.
func (p *scanner) reachable(key string, s *symbol, live map[string]bool) bool {
	for from := range p.refFrom[key] {
		if from == "<file>" {
			return true
		}
		if from == key {
			continue
		}
		fs, known := p.syms[from]
		if !known {
			// An unexported referrer is outside this census; treat it as live
			// rather than guess, so the result stays an under-report.
			return true
		}
		if !live[from] {
			continue
		}
		if fs.Kind == "type" && clusterOf(fs.PkgPath, fs.Kind, fs.Recv, fs.Name) == clusterOf(s.PkgPath, s.Kind, s.Recv, s.Name) {
			continue
		}
		return true
	}
	return s.IfaceRefs > 0
}

// computeClusters records, for each dead symbol, the other dead symbols that
// must go in the same change. A row with a non-empty ClusterWith is not a
// standalone deletion, and reporting it as one invites deleting a leaf and
// leaving its only caller behind.
func (p *scanner) computeClusters() {
	for k, s := range p.syms {
		if s.Live {
			continue
		}
		for from := range p.refFrom[k] {
			if from == k || from == "<file>" {
				continue
			}
			fs := p.syms[from]
			if fs != nil && !fs.Live {
				s.ClusterWith = append(s.ClusterWith, from)
			}
		}
		sort.Strings(s.ClusterWith)
	}
}

// ---- output ----

func (p *scanner) sorted() []*symbol {
	q := make([]*symbol, 0, len(p.syms))
	for _, s := range p.syms {
		q = append(q, s)
	}
	sort.Slice(q, func(i, j int) bool {
		return q[i].Key < q[j].Key
	})
	return q
}

func (p *scanner) writeJSON() {
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", " ")
	_ = enc.Encode(p.sorted())
}

// report prints the census: the totals, the pin breakdown, and the standalone
// dead rows grouped by package. Rows that are not standalone are counted but
// not listed — they are leads for a cluster, not entries on a work list.
func (p *scanner) report() {
	all := p.sorted()
	loc, dead, deadLOC, cluster, positional := 0, 0, 0, 0, 0
	pins := map[string]int{}
	perPkg := map[string][]*symbol{}
	for _, s := range all {
		loc += s.LOC
		if s.Pin != "" {
			pins[s.Pin]++
		}
		if s.Live {
			continue
		}
		dead++
		switch {
		case len(s.ClusterWith) > 0:
			cluster++
		case !s.standalone():
			positional++
		default:
			deadLOC += s.LOC
			perPkg[s.PkgPath] = append(perPkg[s.PkgPath], s)
		}
	}

	fmt.Printf("modules loaded: %d %v\n", len(p.modules), sortedKeys(p.modules))
	if len(p.modules) < 2 {
		fmt.Println("WARNING: one module only — the ext column is zero and the dead list is overstated")
	}
	fmt.Printf("exported symbols: %d (%d LOC)\n", len(all), loc)
	ext := extConsumers(all)
	if len(ext) > 0 {
		shared := ""
		n := extShared(all)
		if n > 0 {
			shared = fmt.Sprintf(" (%d shared)", n)
		}
		fmt.Printf("ext consumers: %d symbols — %s%s\n", extConsumed(all), renderConsumers(ext), shared)
	}
	fmt.Printf("dead: %d — standalone %d (%d LOC), cluster-only %d, iota member %d\n",
		dead, dead-cluster-positional, deadLOC, cluster, positional)
	fmt.Printf("pins: %v\n\n", pins)

	for _, pkg := range sortedPkgs(perPkg) {
		ss := perPkg[pkg]
		n := 0
		for _, s := range ss {
			n += s.LOC
		}
		fmt.Printf("%s  (%d symbols, %d LOC)\n", pkg, len(ss), n)
		for _, s := range ss {
			fmt.Printf("  %-46s %-7s prod=%d test=%d  %s\n",
				s.displayName(), s.Kind, s.ProdRefs, s.TestRefs, s.Pos)
		}
	}
}

// consumerCount is one module and the number of audited symbols it references.
type consumerCount struct {
	Module string
	Syms   int
}

// extConsumers counts, per consuming module, how many audited symbols it
// references, ordered by count and then by name.
//
// Per-module rather than one total because the two kinds of consumer answer
// different questions. Both keep a symbol live; only an INDEPENDENT one is
// evidence that the symbol is public API. On this tree wile-goast accounts for
// nearly all of it, and wile-goast is first-party — it is the analyzer Wile
// runs on its own source, so a reference from it says we use our own internals,
// not that an embedder depends on them.
func extConsumers(all []*symbol) []consumerCount {
	n := map[string]int{}
	for _, s := range all {
		for m := range s.ExtBy {
			n[m]++
		}
	}
	q := make([]consumerCount, 0, len(n))
	for m, c := range n {
		q = append(q, consumerCount{Module: m, Syms: c})
	}
	sort.Slice(q, func(i, j int) bool {
		if q[i].Syms != q[j].Syms {
			return q[i].Syms > q[j].Syms
		}
		return q[i].Module < q[j].Module
	})
	return q
}

// extConsumed counts the symbols with at least one out-of-module consumer. It
// is not the sum of extConsumers: a symbol both modules reference is one
// symbol and two rows.
func extConsumed(all []*symbol) int {
	q := 0
	for _, s := range all {
		if s.ExtRefs > 0 {
			q++
		}
	}
	return q
}

// extShared counts the symbols more than one consuming module references. It is
// what reconciles the per-module rows with the total, which otherwise appear not
// to add up: on this tree 95 + 37 covers 102 symbols because 30 are in both.
//
// It is also the number the public-API question turns on. Subtract it and the
// independent consumer attests only 7 symbols on its own, while 65 rest on
// first-party tooling alone.
func extShared(all []*symbol) int {
	q := 0
	for _, s := range all {
		if len(s.ExtBy) > 1 {
			q++
		}
	}
	return q
}

func renderConsumers(cs []consumerCount) string {
	parts := make([]string, 0, len(cs))
	for _, c := range cs {
		parts = append(parts, fmt.Sprintf("%s %d", c.Module, c.Syms))
	}
	return strings.Join(parts, ", ")
}

func (p *symbol) displayName() string {
	if p.Recv == "" {
		return p.Name
	}
	return p.Recv + "." + p.Name
}

func sortedKeys(m map[string]bool) []string {
	q := make([]string, 0, len(m))
	for k := range m {
		q = append(q, k)
	}
	sort.Strings(q)
	return q
}

func sortedPkgs(m map[string][]*symbol) []string {
	q := make([]string, 0, len(m))
	for k := range m {
		q = append(q, k)
	}
	sort.Strings(q)
	return q
}
