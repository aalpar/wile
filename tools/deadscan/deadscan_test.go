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

package deadscan

import (
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/tools/go/packages"
)

// fixturePkgPath is the package path the fixture is type-checked under. It has
// to sit inside an audited tree, or inScope rejects every declaration in it.
const fixturePkgPath = wileModule + "/pkg/sample"

// loadFixture type-checks testdata/sample.go and wraps it in the
// packages.Package shape the scanner consumes.
//
// go/types does the work rather than go/packages: the fixture imports nothing,
// so a full package load would only add a build step and a dependency on the
// tree's own state. The Module is synthetic and roots at testdata/ so that
// classify sees the fixture at the module top level — rooting it at the repo
// would put it under tools/, which classify correctly calls test support.
func loadFixture(t *testing.T) *packages.Package {
	t.Helper()
	dir, err := filepath.Abs("testdata")
	if err != nil {
		t.Fatalf("abs(testdata): %v", err)
	}
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, filepath.Join(dir, "sample.go"), nil, parser.ParseComments)
	if err != nil {
		t.Fatalf("parse fixture: %v", err)
	}
	info := &types.Info{
		Defs:  map[*ast.Ident]types.Object{},
		Uses:  map[*ast.Ident]types.Object{},
		Types: map[ast.Expr]types.TypeAndValue{},
	}
	conf := types.Config{}
	tp, err := conf.Check(fixturePkgPath, fset, []*ast.File{f}, info)
	if err != nil {
		t.Fatalf("typecheck fixture: %v", err)
	}
	return &packages.Package{
		ID:        "sample",
		PkgPath:   fixturePkgPath,
		Fset:      fset,
		Syntax:    []*ast.File{f},
		Types:     tp,
		TypesInfo: info,
		Module:    &packages.Module{Path: wileModule, Dir: dir},
	}
}

// scanFixture runs every pass over the fixture and returns the symbols keyed by
// their display name (Recv.Name, or Name).
func scanFixture(t *testing.T) map[string]*Symbol {
	t.Helper()
	pkg := loadFixture(t)
	p := newScanner()
	p.run(map[string]*packages.Package{"sample": pkg}, []string{"sample"})
	q := map[string]*Symbol{}
	for _, s := range p.syms {
		q[s.DisplayName()] = s
	}
	return q
}

// TestFixtureLiveness is the whole classifier in one table: every row is a
// classification the tool has to get right, and four of them are the false
// positives the census shipped.
func TestFixtureLiveness(t *testing.T) {
	got := scanFixture(t)
	tcs := []struct {
		name       string
		wantLive   bool
		wantPin    string
		wantIota   int
		wantSolo   bool
		wantWithin []string
		why        string
	}{
		{
			name: "Reachable", wantLive: true, wantSolo: false,
			why: "called from an unexported production function",
		},
		{
			name: "Orphan", wantLive: false, wantSolo: true,
			why: "referenced by nothing at all",
		},
		{
			name: "Leaf", wantLive: false, wantSolo: false,
			wantWithin: []string{"LeafCaller"},
			why:        "reachable only from a dead caller, so not a standalone deletion",
		},
		{
			name: "LeafCaller", wantLive: false, wantSolo: true,
			why: "dead, and the referrer that makes Leaf cluster-only",
		},
		{
			name: "KindFirst", wantLive: false, wantIota: 2, wantSolo: false,
			why: "an iota member: removing it renumbers KindSecond",
		},
		{
			name: "KindSecond", wantLive: false, wantIota: 2, wantSolo: false,
			why: "the other half of the same iota block",
		},
		{
			name: "Checker.Check", wantLive: true, wantPin: "assert",
			why: "pinned by var _ Validator = (*Checker)(nil); deleting it is a compile error",
		},
		{
			name: "Checker.Helper", wantLive: false, wantSolo: true,
			why: "not part of Validator, so the assertion must not pin it",
		},
		{
			name: "FaultError.Error", wantLive: true, wantPin: "error-protocol",
			why: "dispatched through the universe error interface",
		},
		{
			name: "FaultError.Is", wantLive: true, wantPin: "error-protocol",
			why: "the errors package protocol, same pin",
		},
		{
			name: "Zero", wantLive: true,
			why: "read only from the pinned Is; death must not propagate through a pin",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			s := got[tc.name]
			if s == nil {
				t.Fatalf("%s not collected", tc.name)
			}
			if s.Live != tc.wantLive {
				t.Errorf("Live = %v, want %v (%s)", s.Live, tc.wantLive, tc.why)
			}
			if s.Pin != tc.wantPin {
				t.Errorf("Pin = %q, want %q (%s)", s.Pin, tc.wantPin, tc.why)
			}
			if s.IotaGroup != tc.wantIota {
				t.Errorf("IotaGroup = %d, want %d", s.IotaGroup, tc.wantIota)
			}
			if s.Standalone() != tc.wantSolo {
				t.Errorf("standalone = %v, want %v (%s)", s.Standalone(), tc.wantSolo, tc.why)
			}
			for _, want := range tc.wantWithin {
				if !hasSuffix(s.ClusterWith, "."+want) {
					t.Errorf("ClusterWith = %v, want a member ending .%s", s.ClusterWith, want)
				}
			}
		})
	}
}

func hasSuffix(xs []string, suffix string) bool {
	for _, x := range xs {
		if strings.HasSuffix(x, suffix) {
			return true
		}
	}
	return false
}

// TestFixtureUnexportedIsOutOfScope pins the documented limit: the filter is
// ast.IsExported, so an unexported referrer is never itself a row, and the
// fixpoint has to treat it as live rather than guess.
func TestFixtureUnexportedIsOutOfScope(t *testing.T) {
	got := scanFixture(t)
	_, ok := got["reachableCaller"]
	if ok {
		t.Fatal("reachableCaller is unexported and must not be collected")
	}
	s := got["Reachable"]
	if s == nil || !s.Live {
		t.Fatal("Reachable must stay live through its unexported caller")
	}
}

func TestUsesIotaAndCountNames(t *testing.T) {
	tcs := []struct {
		name      string
		src       string
		wantIota  bool
		wantNames int
	}{
		{"iota block", "package p\nconst (\n A = iota\n B\n)\n", true, 2},
		{"plain block", "package p\nconst (\n A = 1\n B = 2\n)\n", false, 2},
		{"multi-name spec", "package p\nconst (\n A, B = iota, iota\n C = 3\n)\n", true, 3},
		{"single const", "package p\nconst A = 1\n", false, 1},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			d := firstGenDecl(t, tc.src)
			if usesIota(d) != tc.wantIota {
				t.Errorf("usesIota = %v, want %v", usesIota(d), tc.wantIota)
			}
			if countNames(d) != tc.wantNames {
				t.Errorf("countNames = %d, want %d", countNames(d), tc.wantNames)
			}
		})
	}
}

func firstGenDecl(t *testing.T, src string) *ast.GenDecl {
	t.Helper()
	f, err := parser.ParseFile(token.NewFileSet(), "x.go", src, 0)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	for _, d := range f.Decls {
		gd, ok := d.(*ast.GenDecl)
		if ok {
			return gd
		}
	}
	t.Fatal("no GenDecl in source")
	return nil
}

// TestClassify covers the rule that broke when it was written against an
// absolute path: classification is relative to the file's own MODULE root, so a
// checkout under any directory name gives the same answer.
func TestClassify(t *testing.T) {
	root := filepath.FromSlash("/anywhere/at/all")
	inModule := &packages.Package{Module: &packages.Module{Path: wileModule, Dir: root}}
	elsewhere := &packages.Package{Module: &packages.Module{Path: "github.com/aalpar/wile-goast", Dir: root}}
	tcs := []struct {
		name string
		pkg  *packages.Package
		file string
		want string
	}{
		{"production source", inModule, "pkg/machine/machine_context.go", "prod"},
		{"a test file", inModule, "pkg/machine/machine_context_test.go", "test"},
		{"integration tree", inModule, "integration/r7rs_test_suite.go", "test"},
		{"tools tree", inModule, "tools/cmd/deadscan/main.go", "test"},
		{"a test-helper package", inModule, "pkg/registry/testhelpers/helpers.go", "test"},
		{"another module", elsewhere, "goastcfg/mapper.go", "ext"},
		{"no module at all", &packages.Package{}, "runtime/proc.go", "ext"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			abs := filepath.Join(root, filepath.FromSlash(tc.file))
			got := classify(tc.pkg, abs)
			if got != tc.want {
				t.Errorf("classify(%s) = %q, want %q", tc.file, got, tc.want)
			}
		})
	}
}

func TestInScope(t *testing.T) {
	tcs := []struct {
		path string
		want bool
	}{
		{wileModule + "/pkg/machine", true},
		{wileModule + "/extensions/math", true},
		{wileModule + "/cmd/wile", true},
		{wileModule + "/tools/cmd/deadscan", false},
		{wileModule, false},
		{"github.com/aalpar/wile-goast/goastcfg", false},
		{"strings", false},
	}
	for _, tc := range tcs {
		t.Run(tc.path, func(t *testing.T) {
			if inScope(tc.path) != tc.want {
				t.Errorf("inScope(%q) = %v, want %v", tc.path, inScope(tc.path), tc.want)
			}
		})
	}
}

// TestPinRanking pins the ordering rule: a symbol carrying several reasons
// reports the most specific, and assert wins because it is a compile error
// rather than a behaviour change.
func TestPinRanking(t *testing.T) {
	tcs := []struct {
		name  string
		apply []string
		want  string
	}{
		{"none", nil, ""},
		{"single", []string{"anon-iface"}, "anon-iface"},
		{"assert wins over error", []string{"error-protocol", "assert"}, "assert"},
		{"order does not matter", []string{"assert", "error-protocol"}, "assert"},
		{"external over anon", []string{"anon-iface", "external-iface"}, "external-iface"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			s := &Symbol{}
			for _, r := range tc.apply {
				s.pin(r)
			}
			if s.Pin != tc.want {
				t.Errorf("Pin = %q, want %q", s.Pin, tc.want)
			}
		})
	}
}

func TestStandalone(t *testing.T) {
	tcs := []struct {
		name string
		sym  Symbol
		want bool
	}{
		{"live", Symbol{Live: true}, false},
		{"dead and alone", Symbol{}, true},
		{"iota member", Symbol{IotaGroup: 3}, false},
		{"lone const in an iota block", Symbol{IotaGroup: 1}, true},
		{"has a dead referrer", Symbol{ClusterWith: []string{"pkg.Caller"}}, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			if tc.sym.Standalone() != tc.want {
				t.Errorf("standalone = %v, want %v", tc.sym.Standalone(), tc.want)
			}
		})
	}
}

// TestExtConsumers pins the aggregation behind the per-module split, including
// the case that makes the two numbers disagree: a symbol both modules reference
// is ONE consumed symbol and TWO module rows, so the rows never sum to the
// total and reporting them as though they did would double-count.
func TestExtConsumers(t *testing.T) {
	const goast = "github.com/aalpar/wile-goast"
	const example = "github.com/aalpar/wile-extension-example"
	tcs := []struct {
		name         string
		syms         []*Symbol
		wantOrder    []ConsumerCount
		wantConsumed int
		wantShared   int
	}{
		{
			name:         "no external consumer",
			syms:         []*Symbol{{}, {ProdRefs: 3}},
			wantOrder:    []ConsumerCount{},
			wantConsumed: 0,
		},
		{
			name: "ordered by count, descending",
			syms: []*Symbol{
				{ExtRefs: 1, ExtBy: map[string]int{example: 1}},
				{ExtRefs: 2, ExtBy: map[string]int{goast: 2}},
				{ExtRefs: 9, ExtBy: map[string]int{goast: 9}},
			},
			wantOrder:    []ConsumerCount{{goast, 2}, {example, 1}},
			wantConsumed: 3,
		},
		{
			name: "a shared symbol is one consumed, two rows",
			syms: []*Symbol{
				{ExtRefs: 4, ExtBy: map[string]int{goast: 3, example: 1}},
			},
			wantOrder:    []ConsumerCount{{example, 1}, {goast, 1}},
			wantConsumed: 1,
			wantShared:   1,
		},
		{
			name: "a tie breaks on the module name",
			syms: []*Symbol{
				{ExtRefs: 1, ExtBy: map[string]int{goast: 1}},
				{ExtRefs: 1, ExtBy: map[string]int{example: 1}},
			},
			wantOrder:    []ConsumerCount{{example, 1}, {goast, 1}},
			wantConsumed: 2,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := ExtConsumers(tc.syms)
			if len(got) != len(tc.wantOrder) {
				t.Fatalf("ExtConsumers = %v, want %v", got, tc.wantOrder)
			}
			for i, w := range tc.wantOrder {
				if got[i] != w {
					t.Errorf("row %d = %v, want %v", i, got[i], w)
				}
			}
			if ExtConsumed(tc.syms) != tc.wantConsumed {
				t.Errorf("ExtConsumed = %d, want %d", ExtConsumed(tc.syms), tc.wantConsumed)
			}
			if ExtShared(tc.syms) != tc.wantShared {
				t.Errorf("ExtShared = %d, want %d", ExtShared(tc.syms), tc.wantShared)
			}
		})
	}
}

// TestExtSplitDoesNotChangeLiveness pins the property that makes the split
// safe: both kinds of consumer root a symbol, so splitting the column is a
// reporting change and never a dead/alive one. Dropping first-party references
// from liveness would report symbols as dead whose deletion breaks wile-goast.
func TestExtSplitDoesNotChangeLiveness(t *testing.T) {
	tcs := []struct {
		name string
		sym  Symbol
	}{
		{"first-party consumer", Symbol{ExtRefs: 1, ExtBy: map[string]int{"github.com/aalpar/wile-goast": 1}}},
		{"independent consumer", Symbol{ExtRefs: 1, ExtBy: map[string]int{"github.com/aalpar/wile-extension-example": 1}}},
	}
	p := newScanner()
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			s := tc.sym
			if !p.rooted(&s) {
				t.Error("an out-of-module consumer must root the symbol whichever module it is")
			}
		})
	}
}

func TestClusterOf(t *testing.T) {
	tcs := []struct {
		name                   string
		pkg, kind, recv, sname string
		want                   string
	}{
		{"method groups with its receiver", "p", "method", "T", "M", "p.T"},
		{"field groups with its struct", "p", "field", "T", "F", "p.T"},
		{"func is its own group", "p", "func", "", "F", "p.F"},
		{"type is its own group", "p", "type", "", "T", "p.T"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := clusterOf(tc.pkg, tc.kind, tc.recv, tc.sname)
			if got != tc.want {
				t.Errorf("clusterOf = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestRelTo(t *testing.T) {
	root := filepath.FromSlash("/a/b")
	tcs := []struct {
		name, root, file, want string
	}{
		{"under the root", root, filepath.Join(root, "c", "d.go"), "c/d.go"},
		{"no root given", "", filepath.FromSlash("/a/b/c.go"), filepath.FromSlash("/a/b/c.go")},
		{"at the root", root, filepath.Join(root, "d.go"), "d.go"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := relTo(tc.root, tc.file)
			if got != tc.want {
				t.Errorf("relTo(%q, %q) = %q, want %q", tc.root, tc.file, got, tc.want)
			}
		})
	}
}
