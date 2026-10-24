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

// PIN, not a gate: a hand-written primitive whose own body calls a values-package
// mutator must carry PrimitiveSpec.Mutates.
//
// Mutation that reddens it: delete `Mutates: true` from the set-car! spec
// (pkg/registry/core/pairs.go). The test then names set-car! and PrimSetCar.
// Second mutation, for the vacuity guards: delete "SetCar" from
// mutationSinkSelectors, or rename the Pair.SetCar method; either fails
// TestMutationSinksAreValuesMethods or the must-discover row.
//
// What it does NOT cover, so green is not read as coverage:
//
//   - Helper-built primitives. An Impl that is a closure returned by a factory
//     (makeComplexPrimitive and the like) joins to its enclosing factory, not to a
//     body that holds the sink, and is skipped by construction. That is 152 of the
//     464 registered primitives with an Impl today (33%), counted as Impls whose
//     runtime name carries a closure suffix; the plan's 103 of 463 counted
//     something narrower. This includes every `helpers.Make...Setter` primitive:
//     %parameter-raw-set! and atomic-store! are annotated and invisible here.
//   - A sink reached through a call. This is a DIRECT-sink arm: only the Impl's own
//     declaration, nested closures included, is scanned. record-modifier is the
//     proof of the limit: PrimRecordModifier hands a function-valued argument to a
//     factory, newRecordModifierClosure, and the closure it returns calls
//     rec.SetField. The factory sits in call.Args and is invoked through a
//     func-typed parameter, so no static call edge reaches it whatever sink set is
//     chosen. record-modifier is annotated, but this pin did not find it. Treating
//     function-valued arguments and func-typed call sites as edges, plus
//     exported-field writes on pkg/values types, is a larger item (TODO.md).
//   - A mutator reached only by applying a returned object ((p v) on a parameter).
//   - Spelling: the sink set is a literal list of method names. A mutator added to
//     pkg/values under a new name is invisible until it is added here;
//     TestMutationSinksAreValuesMethods keeps the list from naming methods that no
//     longer exist, not from missing ones that do.
//
// It does not widen nonDestructiveBangs. Arm B of
// TestNoMutationRemovesEveryDestructivePrimitive iterates only bang-suffixed
// unannotated primitives, which is a different population from this one.

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
// existing Scheme value a program may be holding. Matching is by selector name, as
// in the InvokesProcedure guard (invokes_procedure_guard_test.go): no type checker,
// stdlib only, so Set, Delete and Clear match on any receiver. Measured when this
// list was written: they produce no hit beyond append (see freshConstruction).
// Deliberately omitted, with the measurement that omits them:
//
//   - SetValue: also names MachineContext.SetValue, the VM value register, which
//     nearly every primitive calls. With it in, 239 primitives were demanded.
//   - Store: adds call-with-exit and namespace-undefine!, which hit it on unrelated
//     receivers.
//   - SetSpecific: thread-specific-set! and its siblings are classified
//     non-destructive in nonDestructiveBangs, and this pin must not contradict that.
var mutationSinkSelectors = map[string]bool{
	"SetCar":         true,
	"SetCdr":         true,
	"SetField":       true,
	"SetFieldByName": true,
	"SetChar":        true,
	"Fill":           true,
	"Set":            true,
	"Delete":         true,
	"Clear":          true,
	"Swap":           true,
	"CompareAndSwap": true,
}

// freshConstruction lists hand-written primitives that call a mutator only on cells
// they allocated themselves, so no value a program already holds is touched. The
// reason is the whole entry: an exemption without one is how a real mutator hides.
// A stale entry (the primitive no longer reaches a sink) fails the pin, so this list
// cannot outlive the code it excuses.
var freshConstruction = map[string]string{
	"append": "links freshly allocated cells through a tail pointer via SetCdr; the last argument is spliced in by reference, never written to",
}

// directMutationSinkDecls returns the names of the top-level functions in files
// whose own body, nested closures included, calls a method named in sinks.
func directMutationSinkDecls(files []*ast.File, sinks map[string]bool) map[string]bool {
	q := map[string]bool{}
	for _, f := range files {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if !ok || fd.Body == nil {
				continue
			}
			if bodyCallsSink(fd.Body, sinks) {
				q[fd.Name.Name] = true
			}
		}
	}
	return q
}

func bodyCallsSink(body ast.Node, sinks map[string]bool) bool {
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
		if sinks[sel.Sel.Name] {
			found = true
			return false
		}
		return true
	})
	return found
}

// TestMutatesDirectSinkPin is the pin. See the file header for what reddens it and
// what it does not cover.
func TestMutatesDirectSinkPin(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
	if err != nil {
		t.Fatalf("new engine (profile=KitchenSink, no library paths): %v", err)
	}
	defer func() {
		_ = eng.Close()
	}()

	type primInfo struct {
		name    string
		pkg     string
		fn      string
		mutates bool
	}
	var handWritten []primInfo
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
		handWritten = append(handWritten, primInfo{
			name:    pr.Spec.Name,
			pkg:     pkg,
			fn:      fn,
			mutates: pr.Spec.Mutates,
		})
	}

	root := repoRoot(t)
	fset := token.NewFileSet()
	sinkDecls := map[string]map[string]bool{}
	for pkg := range pkgPaths {
		relDir := strings.TrimPrefix(pkg, modulePath+"/")
		if relDir == pkg {
			t.Logf("skipping non-module primitive package %q", pkg)
			continue
		}
		files := parsePackageFiles(t, fset, filepath.Join(root, relDir))
		sinkDecls[pkg] = directMutationSinkDecls(files, mutationSinkSelectors)
	}

	var missing []string
	discovered := map[string]bool{}
	exemptSeen := map[string]bool{}
	for _, in := range handWritten {
		if !sinkDecls[in.pkg][in.fn] {
			continue
		}
		discovered[in.pkg+"."+in.fn] = true
		_, exempt := freshConstruction[in.name]
		if exempt {
			exemptSeen[in.name] = true
			continue
		}
		if !in.mutates {
			missing = append(missing, in.name+" (Impl "+in.pkg+"."+in.fn+")")
		}
	}
	sort.Strings(missing)
	for _, m := range missing {
		t.Errorf("primitive %s calls a values-package mutator directly but is NOT marked "+
			"Mutates:true. A NoMutation engine would hand it out. Annotate its "+
			"PrimitiveSpec and add the name to mutationPrimitives() "+
			"(TestMutatesMatchesMutationPrimitives pins the two together).", m)
	}

	for name, reason := range freshConstruction {
		if !exemptSeen[name] {
			t.Errorf("freshConstruction exempts %q (%s), but no hand-written primitive of "+
				"that name reaches a sink any more: remove the stale exemption", name, reason)
		}
	}

	// Vacuity guard: the loop above passes trivially if the analysis silently broke
	// (a selector renamed, a package not parsed). One row per sink shape the arm
	// must see, so deleting a selector from mutationSinkSelectors is not "redundant".
	mustDiscover := []string{
		modulePath + "/pkg/registry/core.PrimSetCar",
		modulePath + "/pkg/registry/core.PrimSetCdr",
		modulePath + "/pkg/registry/core.PrimVectorSet",
		modulePath + "/pkg/registry/core.PrimStringSet",
		modulePath + "/pkg/registry/core.PrimListSet",
		modulePath + "/pkg/registry/core.PrimHashtableSet",
		modulePath + "/pkg/registry/core.PrimHashtableDelete",
		modulePath + "/pkg/registry/core.PrimHashtableClear",
		modulePath + "/pkg/internal/extensions/all.PrimStringFill",
	}
	for _, key := range mustDiscover {
		if !discovered[key] {
			t.Errorf("direct-sink scan failed to discover known mutator %s: the pin is "+
				"vacuous or broken (selector renamed? package not analyzed?)", key)
		}
	}
	t.Logf("direct-sink pin: %d hand-written primitives scanned, %d helper-built skipped, %d discovered",
		len(handWritten), helperBuilt, len(discovered))
}

// TestMutationSinksAreValuesMethods keeps the sink list honest in one direction: every
// name in it must still be a method declared in pkg/values, so a rename there fails
// here instead of silently emptying the scan.
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
// package: a hand-written body with a sink is found, so is one with the sink in a
// nested closure, and a body whose sink hides behind a call (the record-modifier
// shape) is NOT, which is the documented limit, pinned so it cannot be quietly
// "fixed" without the header changing.
func TestDirectMutationSinkAnalyzerDetectsSinks(t *testing.T) {
	const src = `
package fake

type pair struct{}

func (*pair) SetCar(v any) {}

func PrimSetCar(p *pair) {
	p.SetCar(nil)
}

func PrimNested(p *pair) func() {
	return func() {
		p.SetCar(nil)
	}
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
	got := directMutationSinkDecls([]*ast.File{f}, mutationSinkSelectors)
	tcs := []struct {
		fn   string
		want bool
	}{
		{"PrimSetCar", true},
		{"PrimNested", true},
		{"PrimViaFactory", false}, // the documented limit: the sink is one call away
		{"newModifier", true},     // the factory itself holds it; nothing reaches it from PrimViaFactory
		{"PrimPure", false},
	}
	for _, tc := range tcs {
		t.Run(tc.fn, func(t *testing.T) {
			if got[tc.fn] != tc.want {
				t.Errorf("directMutationSinkDecls[%s] = %v, want %v", tc.fn, got[tc.fn], tc.want)
			}
		})
	}
}
