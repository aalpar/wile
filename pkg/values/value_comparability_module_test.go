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

package values_test

import (
	"go/importer"
	"go/token"
	"go/types"
	"os"
	"path/filepath"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// valueImplementorPackages are every package in the module that declares a type
// implementing values.Value. Derived by grepping for EqualTo declarations; the test
// FAILS if a package outside this list grows one (see TestValueImplementorPackages_
// ListIsComplete), so the list cannot silently go stale.
var valueImplementorPackages = []string{
	"pkg/values",
	"pkg/syntax",
	"pkg/machine",
	"pkg/machine/compilation",
	"pkg/environment",
	"pkg/parser",
	"pkg/internal/tokenizer",
}

// TestValue_AllImplementorsAreGoComparable is THE enforcement of the values.Value
// Go-comparability contract, module-wide.
//
// THE CONTRACT: every type implementing values.Value must be Go-comparable — usable
// with == and as a map key. It is not decoration. values.EqIdentity (eq?, memq, assq,
// and the literal pool) is a bare `a == b` on an interface, and Go panics with
// "comparing uncomparable type T" when the dynamic type is a slice, map, or func. In an
// embedded engine that panic lands in the HOST's process. Three violators shipped
// undetected (machine.Operations, machine.MultipleValues, machine.boxedValues) and none
// was found by reading code.
//
// WHY go/types AND NOT A ROSTER. Go comparability has no method set, so it cannot be
// stated as an interface and the compiler will not check it. The first attempt was a
// hand-maintained roster per package plus a reflect.Comparable assertion over it, and it
// failed in exactly the way hand-maintained rosters fail: pkg/machine's roster listed 7
// types while the package declares 50 (every Operation* is a values.Value — Operation
// embeds it), and the test named ...CoverPackage scanned nothing. A roster that does not
// cover its package is worse than no test, because it reports green over the gap. Five
// of the seven packages below had no roster at all.
//
// go/types answers both halves — "does T implement values.Value" and "is T comparable" —
// from source, with no roster to maintain, no reflect, and no dependency outside the
// standard library. It is the only form of this check that cannot go stale.
//
// The cost is ~1s per package to type-check from source. That is why this is one test
// over a fixed package list rather than a per-package test.
func TestValue_AllImplementorsAreGoComparable(t *testing.T) {
	fset := token.NewFileSet()
	// ONE importer for everything, and every package comes from it — including
	// pkg/values itself. Type identity in go/types is per *types.Package instance: if
	// values.Value is resolved once by this importer and again by a separate
	// types.Config.Check, the two Value interfaces are DIFFERENT types, every
	// EqualTo(values.Value) signature fails to match, and types.Implements answers false
	// for all ~90 implementors. The suite then passes over an empty set. That is what
	// the anti-vacuity floor below is for, and it caught exactly this while this test
	// was being written.
	imp := importer.ForCompiler(fset, "source", nil)

	valIface := lookupValueInterface(t, imp)

	total := 0
	for _, rel := range valueImplementorPackages {
		pkg, err := imp.Import("github.com/aalpar/wile/" + rel)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("import %s", rel))
		scope := pkg.Scope()
		for _, name := range scope.Names() {
			obj, ok := scope.Lookup(name).(*types.TypeName)
			if !ok {
				continue
			}
			named, ok := obj.Type().(*types.Named)
			if !ok {
				continue
			}
			// A type may implement Value on its value form or its pointer form. The
			// RECEIVER decides, and that is the trap the contract exists around: a slice
			// type with VALUE receivers is a legal Value and a guaranteed panic.
			for _, cand := range []types.Type{named, types.NewPointer(named)} {
				if !types.Implements(cand, valIface) {
					continue
				}
				total++
				qt.Assert(t, types.Comparable(cand), qt.IsTrue, qt.Commentf(
					"%s implements values.Value but is NOT Go-comparable.\n"+
						"values.EqIdentity (eq?, memq, assq) is a bare `a == b` on the interface, "+
						"so this panics with \"comparing uncomparable type\" in the host's process.\n"+
						"Fix: give the type POINTER receivers (the receiver decides, not the "+
						"underlying type), or stop implementing values.Value if it is not a Scheme datum.",
					cand))
				break
			}
		}
	}

	// Anti-vacuity. A refactor that breaks the type-check, renames the interface, or
	// mis-resolves the package would otherwise leave this test green over an empty set —
	// the exact failure mode it exists to prevent. The real count is ~90.
	qt.Assert(t, total > 50, qt.IsTrue, qt.Commentf(
		"only %d values.Value implementors found across %d packages; expected far more. "+
			"The scan is not seeing the types it is meant to guard.",
		total, len(valueImplementorPackages)))
	t.Logf("checked %d values.Value implementors across %d packages", total, len(valueImplementorPackages))
}

// TestValueImplementorPackages_ListIsComplete keeps valueImplementorPackages honest.
//
// The package LIST above is the one hand-maintained thing left, so it gets the same
// treatment a roster would: a source scan proves nothing is missing from it. Any package
// that declares an EqualTo(values.Value) method — the distinguishing member of the Value
// interface — must be listed, or its types go unguarded.
func TestValueImplementorPackages_ListIsComplete(t *testing.T) {
	root := moduleRoot(t)
	listed := make(map[string]bool, len(valueImplementorPackages))
	for _, p := range valueImplementorPackages {
		listed[p] = true
	}

	var missing []string
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "testdata" || d.Name() == ".git" || d.Name() == "dist" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		// Both spellings: "values.Value" from other packages, bare "Value" inside
		// pkg/values itself.
		if !strings.Contains(string(src), ") EqualTo(") {
			return nil
		}
		if !strings.Contains(string(src), "values.Value) bool") && !strings.Contains(string(src), "Value) bool") {
			return nil
		}
		rel, err := filepath.Rel(root, filepath.Dir(path))
		if err != nil {
			return err
		}
		if !listed[rel] {
			missing = append(missing, rel+" (in "+filepath.Base(path)+")")
		}
		return nil
	})
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, missing, qt.HasLen, 0, qt.Commentf(
		"these packages declare an EqualTo(values.Value) method but are absent from "+
			"valueImplementorPackages, so their types are NOT checked for Go-comparability. "+
			"Add them to the list: %v", missing))
}

func moduleRoot(t *testing.T) string {
	t.Helper()
	// The test runs in pkg/values; the module root is two levels up.
	root, err := filepath.Abs(filepath.Join("..", ".."))
	qt.Assert(t, err, qt.IsNil)
	_, err = os.Stat(filepath.Join(root, "go.mod"))
	qt.Assert(t, err, qt.IsNil, qt.Commentf("expected go.mod at %s", root))
	return root
}

func lookupValueInterface(t *testing.T, imp types.Importer) *types.Interface {
	t.Helper()
	pkg, err := imp.Import("github.com/aalpar/wile/pkg/values")
	qt.Assert(t, err, qt.IsNil)
	obj := pkg.Scope().Lookup("Value")
	qt.Assert(t, obj, qt.IsNotNil, qt.Commentf("values.Value not found — has it been renamed?"))
	iface, ok := obj.Type().Underlying().(*types.Interface)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("values.Value is not an interface"))
	return iface
}
