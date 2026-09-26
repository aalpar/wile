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

package wile_test

import (
	"context"
	"io/fs"
	"path"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// stdlibLibraryNames derives every library name the embedded stdlib ships, from
// the filesystem rather than from a hand-kept list, so a library added later is
// swept without anyone remembering to add it here. stdlib.FS is already
// fs.Sub'd past the "lib" prefix, so "scheme/base.sld" is (scheme base).
func stdlibLibraryNames(t *testing.T) []string {
	t.Helper()
	var q []string
	err := fs.WalkDir(stdlib.FS, ".", func(p string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || path.Ext(p) != ".sld" {
			return nil
		}
		q = append(q, "("+strings.Join(strings.Split(strings.TrimSuffix(p, ".sld"), "/"), " ")+")")
		return nil
	})
	qt.Assert(t, err, qt.IsNil)
	return q
}

// TestExportRootIsOneAnswerPerBinding is the observable that pays for leaving
// stampLibraryExportOrigins' loop unsorted while both library copy loops were
// sorted (I143).
//
// That loop walks lib.Exports in Go map order and stamps each export's
// provenance root through a nil-guarded, first-writer-wins UpdateMeta. Map order
// is therefore unobservable exactly while no two of a library's export keys
// resolve to ONE *Binding with DIFFERENT exportRoot answers. The base arm cannot
// differ — it carries no RootPhase — so only the library-scoped arm can, and the
// claim is that nothing in the tree exhibits it.
//
// That is a claim about the tree, not a theorem, so it is measured here instead
// of being bought with a sort. If this goes red, the position is false and
// stampLibraryExportOrigins needs sortedExportKeys after all; fix it there
// rather than by relaxing this assertion.
//
// In pkg/wile, not in pkg/machine/compilation where the loop lives: the
// population that makes the claim worth checking is the loaded stdlib, and
// loading it needs bootstrap, which imports compilation.
func TestExportRootIsOneAnswerPerBinding(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	c.Assert(err, qt.IsNil)

	// Import every stdlib library so the registry holds them. A library that
	// does not import standalone is skipped rather than failed: this pin is
	// about export-root agreement, and importability is other tests' subject.
	names := stdlibLibraryNames(t)
	c.Assert(len(names), qt.Not(qt.Equals), 0)
	imported := 0
	for _, name := range names {
		_, err := eng.EvalMultiple(ctx, "(import "+name+")")
		if err != nil {
			t.Logf("skipped %s: %v", name, err)
			continue
		}
		imported++
	}

	searcher := eng.Environment().LibraryRegistry()
	reg, ok := searcher.(*compilation.LibraryRegistry)
	c.Assert(ok, qt.IsTrue, qt.Commentf("library registry is %T, not *compilation.LibraryRegistry", searcher))

	libs := reg.All()

	// Guard against a vacuous pass: a sweep that resolved nothing would be green
	// for the wrong reason. The floors are well under today's counts (62 .sld
	// files, all importable) so an added library cannot trip them, but a wiring
	// break that empties the registry or the resolver will.
	c.Assert(imported, qt.Not(qt.Equals), 0, qt.Commentf("no stdlib library imported; the sweep would be vacuous"))
	c.Assert(len(libs) >= 20, qt.IsTrue, qt.Commentf("only %d libraries loaded, expected the stdlib", len(libs)))

	resolved := 0
	libraryScoped := 0
	for _, lib := range libs {
		// One entry per distinct *Binding this library's exports reach, holding
		// the first key that reached it and the root that key computed.
		type witness struct {
			key  compilation.ExportKey
			root environment.OriginRef
		}
		seen := map[*environment.Binding]witness{}

		for key, internalName := range lib.Exports {
			binding, root, found := lib.ExportedBindingRoot(internalName, key.Phase)
			if !found {
				continue
			}
			resolved++
			if root.RootLib != environment.BaseOriginLib {
				libraryScoped++
			}
			prev, dup := seen[binding]
			if !dup {
				seen[binding] = witness{key: key, root: *root}
				continue
			}
			// Compare by value: exportRoot mints a fresh *OriginRef per call.
			c.Check(*root, qt.Equals, prev.root, qt.Commentf(
				"library %s: export keys %+v and %+v resolve to ONE binding but compute different roots, "+
					"so stampLibraryExportOrigins' unsorted first-writer-wins walk is nondeterministic; "+
					"give it sortedExportKeys", lib.Name.SchemeString(), prev.key, key))
		}
	}

	c.Assert(resolved >= 100, qt.IsTrue, qt.Commentf("only %d export keys resolved; the sweep is too thin to support the position", resolved))
	// The base arm provably cannot disagree (no RootPhase), so the position is
	// only interesting where the library-scoped arm is exercised. If this floor
	// ever fails the sweep has stopped covering the arm it exists to cover.
	c.Assert(libraryScoped >= 1, qt.IsTrue, qt.Commentf("no library-scoped export root seen across %d resolved keys; the sweep misses the only arm that can differ", resolved))
	t.Logf("swept %d libraries, %d resolved export keys, %d library-scoped roots", len(libs), resolved, libraryScoped)
}
