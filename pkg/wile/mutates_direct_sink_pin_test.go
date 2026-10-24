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

// PIN, not a gate: a hand-written primitive whose own body calls a method named like a
// values-package mutator must carry PrimitiveSpec.Mutates.
//
// Mutation that reddens it: delete `Mutates: true` from the set-car! spec
// (pkg/registry/core/pairs.go). The test then names set-car!, PrimSetCar, the matched
// selector and its position. Second mutation, for the vacuity guards: delete "SetCar"
// from mutationSinkSelectors; the census and four rows of
// TestDirectMutationSinkAnalyzerDetectsSinks fail. Renaming the Pair.SetCar method
// (callers updated with it) fails TestMutationSinksAreValuesMethods, which catches only
// a rename, and stays green when SetCar is deleted from the list.
//
// What this pin is for. For primitives that exist today, the guard is the drift test
// (dialect_nomutation_drift_test.go): it ties the annotation to the NoMutation
// removal list in both directions. This pin's value is NEW primitives: a new
// hand-written primitive with a direct sink and no annotation fails here, where the
// drift test cannot see it because it derives nothing from the code. Measured
// 2026-10-23: 21 primitives are annotated, and this pin sees 12 of them (13 discovered
// less the one exemption). It is a floor on the easy shape, and green is not coverage.
//
// What it does NOT see, so green is not read as "no unannotated mutator exists":
//
//   - Helper-built primitives. An Impl that is a closure returned by a factory
//     (every helpers.Make...Setter among them) joins to its enclosing factory, not to a
//     body that holds the sink, and is skipped by construction: 152 of the 464
//     registered primitives with an Impl (33%), counted as Impls whose runtime name
//     carries a closure suffix. The plan (defect-closeout-P8.md, P8.11) said 103 of 463,
//     counting something narrower. %parameter-raw-set!, atomic-store! and set-box!
//     (each built with helpers.MakeBinarySetter) are annotated and invisible here for
//     this reason.
//   - A sink reached through a call. Only the Impl's own declaration, nested closures
//     included, is scanned. record-modifier is the proof: PrimRecordModifier hands a
//     function-valued argument to a factory, newRecordModifierClosure, and the closure
//     it returns calls rec.SetField. The factory sits in call.Args and is invoked through
//     a func-typed parameter, so no static edge reaches it. Helper indirection in
//     general is invisible, including the method value `f := p.SetCar; f(v)` (the call
//     is on an identifier, not a selector).
//   - Write-through shapes that call no mutator method: `x.Elems()[i] = v` and
//     `copy(x.Elems(), src)` write into the backing slice directly. This is how
//     vector-fill!, vector-copy!, bytevector-copy!, read-bytevector!
//     (pkg/extensions/io/prim_binary.go) and string-copy! mutate, and none is found.
//   - String.SetValue, whose name is excluded (see mutationSinkSelectors).
//   - Receivers are matched by selector name only, with no type information, so a
//     receiver typed only by identifier or a type assertion `x.(*pair).SetCar(v)` is
//     matched on the method name, and a same-named method on a non-values type is a
//     false positive (the reason for the exemption list).
//   - A mutator reached only by applying a returned object ((p v) on a parameter).
//   - Spelling: the sink set is a literal list. A mutator added to pkg/values under a
//     new name is invisible until added here; TestMutationSinksAreValuesMethods keeps
//     the list from naming methods that no longer exist, not from missing ones that do.
//
// Widening to the write-through and function-argument shapes is a different, larger
// item: todo/tier5-tech-debt.md, "`PrimitiveSpec.Mutates` has no static guard". It does
// not widen nonDestructiveBangs: Arm B of TestNoMutationRemovesEveryDestructivePrimitive
// iterates only bang-suffixed unannotated primitives, a different population.

package wile

import (
	"context"
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

// mutationSinkSelectors are the pkg/values methods that destructively update an
// existing Scheme value a program may be holding. Matching is by selector name, as in
// the InvokesProcedure guard (invokes_procedure_guard_test.go): no type checker, stdlib
// only, so Set, Delete and Clear match on any receiver. Every kept selector has a
// witness: some discovered primitive calls it (TestMutatesDirectSinkPin asserts that, so
// a selector nothing exercises cannot stay in the list). The Set, Delete and Clear hits
// are real values receivers (vectors, strings, lists, hashtables).
//
// Omitted, with the measurement that omits them:
//
//   - SetField and SetFieldByName: Record mutators with no witness among hand-written
//     Impls (record-modifier reaches SetField through a factory this scan cannot
//     follow), so listing them would be an untested claim.
//   - SetValue: also names MachineContext.SetValue, the VM value register, which nearly
//     every primitive calls. With it in, 239 primitives were demanded.
//   - Store: adds call-with-exit and namespace-undefine!, which hit it on unrelated
//     receivers.
//   - SetSpecific: thread-specific-set! and its siblings are classified non-destructive
//     in nonDestructiveBangs, and this pin must not contradict that.
var mutationSinkSelectors = map[string]bool{
	"SetCar":         true,
	"SetCdr":         true,
	"SetChar":        true,
	"Fill":           true,
	"Set":            true,
	"Delete":         true,
	"Clear":          true,
	"Swap":           true,
	"CompareAndSwap": true,
}

// freshConstruction lists hand-written primitives that call a sink method only on cells
// they allocated themselves, so no value a program already holds is touched. The reason
// is the whole entry: an exemption without one is how a real mutator hides. It is per
// NAME, so it would also hide a second, genuine sink added to the same Impl: reviewing
// an edit to PrimAppend means rereading this entry. A stale entry fails the pin both
// ways: the primitive no longer reaches a sink, or it has since been annotated Mutates.
var freshConstruction = map[string]string{
	"append": "links freshly allocated cells through a tail pointer via SetCdr; the last argument is spliced in by reference, never written to",
}

// sinkHit is one call to a sink selector found in a declaration body.
type sinkHit struct {
	selector string
	pos      token.Position
}

// directMutationSinkDecls returns, for each top-level function in files whose own body
// (nested closures included) calls a method named in sinks, the first such call in
// source order and the set of distinct selectors it calls.
func directMutationSinkDecls(fset *token.FileSet, files []*ast.File, sinks map[string]bool) map[string][]sinkHit {
	q := map[string][]sinkHit{}
	for _, f := range files {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if !ok || fd.Body == nil {
				continue
			}
			hits := bodySinkHits(fset, fd.Body, sinks)
			if len(hits) > 0 {
				q[fd.Name.Name] = hits
			}
		}
	}
	return q
}

// bodySinkHits returns one hit per distinct sink selector in body, in source order.
func bodySinkHits(fset *token.FileSet, body ast.Node, sinks map[string]bool) []sinkHit {
	var q []sinkHit
	seen := map[string]bool{}
	ast.Inspect(body, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		sel, ok := call.Fun.(*ast.SelectorExpr)
		if !ok {
			return true
		}
		name := sel.Sel.Name
		if !sinks[name] || seen[name] {
			return true
		}
		seen[name] = true
		q = append(q, sinkHit{selector: name, pos: fset.Position(sel.Sel.Pos())})
		return true
	})
	return q
}

// sinkPrim is what the pin decides over: a hand-written primitive, whether its spec
// carries Mutates, and the sink calls found in its Impl's own body.
type sinkPrim struct {
	name    string
	key     string
	mutates bool
	hits    []sinkHit
}

// mutatesPinFindings is the pin's decision, factored out so it can be driven by fake
// primitives: an unannotated primitive that reaches a sink and is not exempt is a
// finding, and an exemption that is stale (no sink reached, or the primitive is
// annotated) is a finding.
func mutatesPinFindings(prims []sinkPrim, exempt map[string]string) []string {
	var q []string
	exemptSeen := map[string]bool{}
	for _, in := range prims {
		_, isExempt := exempt[in.name]
		if isExempt {
			if len(in.hits) > 0 && !in.mutates {
				exemptSeen[in.name] = true
			}
			continue
		}
		if len(in.hits) == 0 || in.mutates {
			continue
		}
		h := in.hits[0]
		q = append(q, "primitive "+in.name+" (Impl "+in.key+") calls a method named "+h.selector+
			" at "+h.pos.String()+", the name of a values-package mutator, but is NOT marked "+
			"Mutates:true. A NoMutation engine would hand it out. Annotate its PrimitiveSpec and "+
			"add the name to mutationPrimitives() (TestMutatesMatchesMutationPrimitives pins the "+
			"two together). If it only writes cells it allocated itself, add it to "+
			"freshConstruction with the reason.")
	}
	for name, reason := range exempt {
		if !exemptSeen[name] {
			q = append(q, "freshConstruction exempts "+name+" ("+reason+"), but no unannotated "+
				"hand-written primitive of that name reaches a sink any more: remove the stale exemption")
		}
	}
	sort.Strings(q)
	return q
}

// TestMutatesDirectSinkPin is the pin. See the file header for what reddens it and what
// it does not cover.
func TestMutatesDirectSinkPin(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
	if err != nil {
		t.Fatalf("new engine (profile=KitchenSink, no library paths): %v", err)
	}
	defer func() {
		_ = eng.Close()
	}()

	type handWritten struct {
		name    string
		pkg     string
		fn      string
		mutates bool
	}
	var prims []handWritten
	helperBuilt := 0
	pkgPaths := map[string]bool{}
	for _, pr := range eng.Registry().Primitives() {
		goFn, _, _, rErr := resolveImpl(pr.Spec.Impl)
		if rErr != nil {
			t.Errorf("resolveImpl(%q): %v", pr.Spec.Name, rErr)
			continue
		}
		if goFn == "" {
			continue // binding-only primitive (nil Impl)
		}
		pkg, fn := splitQualified(goFn)
		if closureSuffix.MatchString(fn) {
			helperBuilt++
			continue
		}
		pkgPaths[pkg] = true
		prims = append(prims, handWritten{name: pr.Spec.Name, pkg: pkg, fn: fn, mutates: pr.Spec.Mutates})
	}

	root := repoRoot(t)
	fset := token.NewFileSet()
	sinkDecls := map[string]map[string][]sinkHit{}
	for pkg := range pkgPaths {
		relDir := strings.TrimPrefix(pkg, modulePath+"/")
		if relDir == pkg {
			t.Logf("skipping non-module primitive package %q", pkg)
			continue
		}
		files := parsePackageFiles(t, fset, filepath.Join(root, relDir))
		sinkDecls[pkg] = directMutationSinkDecls(fset, files, mutationSinkSelectors)
	}

	var sinkPrims []sinkPrim
	discovered := map[string]bool{}
	witnessed := map[string]bool{}
	for _, in := range prims {
		hits := sinkDecls[in.pkg][in.fn]
		sinkPrims = append(sinkPrims, sinkPrim{name: in.name, key: in.pkg + "." + in.fn, mutates: in.mutates, hits: hits})
		if len(hits) == 0 {
			continue
		}
		discovered[in.name] = true
		for _, h := range hits {
			witnessed[h.selector] = true
		}
	}

	for _, f := range mutatesPinFindings(sinkPrims, freshConstruction) {
		t.Error(f)
	}

	// Census: the exact set of hand-written primitives that reach a sink. A new entry
	// is either a primitive to annotate (the finding above fires first if it is not) or
	// one to exempt; an entry that disappears means the scan lost sight of it. Either
	// way the author updates this list on purpose.
	wantDiscovered := []string{
		"append",
		"atomic-compare-and-swap!",
		"atomic-swap!",
		"bytevector-u8-set!",
		"hashtable-clear!",
		"hashtable-delete!",
		"hashtable-set!",
		"list-set!",
		"set-car!",
		"set-cdr!",
		"string-fill!",
		"string-set!",
		"vector-set!",
	}
	var gotDiscovered []string
	for name := range discovered {
		gotDiscovered = append(gotDiscovered, name)
	}
	sort.Strings(gotDiscovered)
	if strings.Join(gotDiscovered, " ") != strings.Join(wantDiscovered, " ") {
		t.Errorf("the set of hand-written primitives reaching a direct sink changed.\n got: %v\nwant: %v\n"+
			"A new name must be annotated Mutates:true (or exempted in freshConstruction with a "+
			"reason) and added to wantDiscovered; a missing name means the scan lost sight of it "+
			"(selector renamed? package not analyzed?).", gotDiscovered, wantDiscovered)
	}

	// Every kept selector must have a witness, so the list makes no untested claim.
	for sel := range mutationSinkSelectors {
		if !witnessed[sel] {
			t.Errorf("sink selector %q is called by no hand-written primitive: either drop it "+
				"from mutationSinkSelectors or the scan no longer reaches its witness", sel)
		}
	}
	t.Logf("direct-sink pin: %d hand-written primitives scanned, %d helper-built skipped, %d discovered",
		len(prims), helperBuilt, len(discovered))
}

// TestMutatesPinFindingsDecision drives the pin's decision with fake primitives, so a
// pin that always passes cannot hide behind the real registry: an unannotated primitive
// with a sink is a finding, an annotated one is not, an exempt one is not, and a stale
// exemption is a finding both ways.
func TestMutatesPinFindingsDecision(t *testing.T) {
	hit := []sinkHit{{selector: "SetCar", pos: token.Position{Filename: "fake.go", Line: 7, Column: 3}}}
	tcs := []struct {
		name   string
		prims  []sinkPrim
		exempt map[string]string
		want   []string // substrings, one per expected finding
	}{
		{
			name:  "unannotated primitive with a sink is a finding, naming selector and position",
			prims: []sinkPrim{{name: "fake-set!", key: "x.PrimFake", hits: hit}},
			want:  []string{"fake-set! (Impl x.PrimFake) calls a method named SetCar at fake.go:7:3"},
		},
		{
			name:  "annotated primitive with a sink is clean",
			prims: []sinkPrim{{name: "fake-set!", key: "x.PrimFake", mutates: true, hits: hit}},
		},
		{
			name:  "unannotated primitive with no sink is clean",
			prims: []sinkPrim{{name: "fake-ref", key: "x.PrimRef"}},
		},
		{
			name:   "exempt primitive that reaches a sink is clean",
			prims:  []sinkPrim{{name: "fresh", key: "x.PrimFresh", hits: hit}},
			exempt: map[string]string{"fresh": "allocates its own cells"},
		},
		{
			name:   "exemption stale: the primitive no longer reaches a sink",
			prims:  []sinkPrim{{name: "fresh", key: "x.PrimFresh"}},
			exempt: map[string]string{"fresh": "allocates its own cells"},
			want:   []string{"freshConstruction exempts fresh"},
		},
		{
			name:   "exemption stale: the primitive has since been annotated",
			prims:  []sinkPrim{{name: "fresh", key: "x.PrimFresh", mutates: true, hits: hit}},
			exempt: map[string]string{"fresh": "allocates its own cells"},
			want:   []string{"freshConstruction exempts fresh"},
		},
		{
			name:   "exemption stale: no such primitive",
			exempt: map[string]string{"gone": "allocates its own cells"},
			want:   []string{"freshConstruction exempts gone"},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := mutatesPinFindings(tc.prims, tc.exempt)
			if len(got) != len(tc.want) {
				t.Fatalf("got %d findings %q, want %d matching %q", len(got), got, len(tc.want), tc.want)
			}
			for i, w := range tc.want {
				if !strings.Contains(got[i], w) {
					t.Errorf("finding %d = %q, want it to contain %q", i, got[i], w)
				}
			}
		})
	}
}

// TestMutationSinksAreValuesMethods keeps the sink list honest in one direction: every
// name in it must still be a method declared in pkg/values, so a rename there fails here
// instead of silently emptying the scan.
func TestMutationSinksAreValuesMethods(t *testing.T) {
	fset := token.NewFileSet()
	files := parsePackageFiles(t, fset, filepath.Join(repoRoot(t), "pkg", "values"))
	methods := map[string]bool{}
	for _, f := range files {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if !ok || fd.Recv == nil {
				continue
			}
			methods[fd.Name.Name] = true
		}
	}
	for name := range mutationSinkSelectors {
		if !methods[name] {
			t.Errorf("mutationSinkSelectors names %q, which is not a method declared in pkg/values", name)
		}
	}
}

// TestDirectMutationSinkAnalyzerDetectsSinks proves the scan has teeth on a synthetic
// package, and pins the documented misses so they cannot be quietly "fixed" without the
// header changing: a sink one call away, a method value, and a write-through to the
// backing slice are NOT found; a sink in a nested closure and one on a type-asserted
// receiver ARE.
func TestDirectMutationSinkAnalyzerDetectsSinks(t *testing.T) {
	const src = `
package fake

type pair struct{}

func (*pair) SetCar(v any) {}

type vec struct{ elems []any }

func (v *vec) Elems() []any { return v.elems }

func PrimSetCar(p *pair) {
	p.SetCar(nil)
}

func PrimNested(p *pair) func() {
	return func() {
		p.SetCar(nil)
	}
}

func PrimTypeAsserted(x any) {
	x.(*pair).SetCar(nil)
}

func PrimMethodValue(p *pair) {
	f := p.SetCar
	f(nil)
}

func PrimWriteThrough(v *vec) {
	v.Elems()[0] = nil
	copy(v.Elems(), nil)
}

func PrimViaFactory(p *pair) any {
	return newModifier(p)
}

func newModifier(p *pair) func() {
	return func() {
		p.SetCar(nil)
	}
}

func PrimPure() int {
	return 42
}
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "fake.go", src, 0)
	if err != nil {
		t.Fatalf("parse synthetic source: %v", err)
	}
	got := directMutationSinkDecls(fset, []*ast.File{f}, mutationSinkSelectors)
	tcs := []struct {
		fn   string
		want bool
	}{
		{"PrimSetCar", true},
		{"PrimNested", true},
		{"PrimTypeAsserted", true},  // matched on the method name; the receiver's type is never consulted
		{"PrimMethodValue", false},  // documented miss: the call is on an identifier, not a selector
		{"PrimWriteThrough", false}, // documented miss: writes the backing slice, calls no mutator method
		{"PrimViaFactory", false},   // documented miss: the sink is one call away
		{"newModifier", true},       // the factory itself holds it; nothing reaches it from PrimViaFactory
		{"PrimPure", false},
	}
	for _, tc := range tcs {
		t.Run(tc.fn, func(t *testing.T) {
			if (len(got[tc.fn]) > 0) != tc.want {
				t.Errorf("sink hits for %s = %v, want found=%v", tc.fn, got[tc.fn], tc.want)
			}
		})
	}
}
