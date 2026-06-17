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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
	"github.com/aalpar/wile/stdlib"
)

// TestLibraryName_String pins the public projection type's Scheme-form
// rendering. LibraryName is the owned wile-package type that AvailableLibraries
// returns instead of leaking machine/compilation.LibraryName (R19).
func TestLibraryName_String(t *testing.T) {
	name := wile.LibraryName{Parts: []string{"scheme", "base"}}
	qt.Assert(t, name.String(), qt.Equals, "(scheme base)")

	single := wile.LibraryName{Parts: []string{"wile"}}
	qt.Assert(t, single.String(), qt.Equals, "(wile)")
}

// TestEngine_AvailableLibraries_OwnedType verifies AvailableLibraries returns
// the owned []wile.LibraryName (no machine/compilation type in the signature),
// with usable Parts and a Scheme-form String. The owned Parts slice is a copy,
// so mutating it cannot reach engine state.
func TestEngine_AvailableLibraries_OwnedType(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	libs, libErr := eng.AvailableLibraries(ctx)
	qt.Assert(t, libErr, qt.IsNil)
	qt.Assert(t, len(libs) > 0, qt.IsTrue)

	var foundWile bool
	for _, lib := range libs {
		if len(lib.Parts) >= 1 && lib.Parts[0] == "wile" {
			foundWile = true
			qt.Assert(t, lib.String(), qt.Equals, "("+strings.Join(lib.Parts, " ")+")",
				qt.Commentf("String() should render the Scheme form"))
		}
	}
	qt.Assert(t, foundWile, qt.IsTrue,
		qt.Commentf("should discover at least one (wile ...) library"))
}

func TestEngine_LoadedLibraries_Empty(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)
	libs, libErr := eng.LoadedLibraries()
	qt.Assert(t, libErr, qt.IsNil)
	qt.Assert(t, len(libs), qt.Equals, 0)
}

func TestEngine_LoadedLibraries_AfterImport(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(import (scheme base))`)
	qt.Assert(t, err, qt.IsNil)

	libs, libErr := eng.LoadedLibraries()
	qt.Assert(t, libErr, qt.IsNil)
	qt.Assert(t, len(libs) > 0, qt.IsTrue)

	var found bool
	for _, lib := range libs {
		if lib.Name == "(scheme base)" {
			found = true
			qt.Assert(t, len(lib.Exports) > 0, qt.IsTrue)
		}
	}
	qt.Assert(t, found, qt.IsTrue,
		qt.Commentf("should find (scheme base) in loaded libraries"))
}

func TestEngine_LookupLibrary_NotFound(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)
	info, lookupErr := eng.LookupLibrary("nonexistent", "lib")
	qt.Assert(t, lookupErr, qt.IsNil)
	qt.Assert(t, info, qt.IsNil)
}

func TestEngine_LookupLibrary_Found(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(import (scheme base))`)
	qt.Assert(t, err, qt.IsNil)

	info, lookupErr := eng.LookupLibrary("scheme", "base")
	qt.Assert(t, lookupErr, qt.IsNil)
	qt.Assert(t, info, qt.IsNotNil)
	qt.Assert(t, info.Name, qt.Equals, "(scheme base)")
}

func TestEngine_UnloadedLibraries(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	libs := eng.UnloadedLibraries(ctx)
	var found bool
	for _, lib := range libs {
		if lib.Name == "(wile algebra)" {
			found = true
		}
	}
	qt.Assert(t, found, qt.IsTrue,
		qt.Commentf("should find (wile algebra) as unloaded"))
}

func TestEngine_UnloadedLibraries_StoresExportIndexOnNamespace(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	// Before any library scan, namespace should have no export index.
	ns := eng.Environment().Namespace()
	idx, built := ns.ExportIndex()
	qt.Assert(t, idx, qt.IsNil)
	qt.Assert(t, built, qt.IsFalse)

	// Trigger the export index build.
	libs := eng.UnloadedLibraries(ctx)
	qt.Assert(t, len(libs) > 0, qt.IsTrue)

	// Namespace should now have the export index.
	idx, built = ns.ExportIndex()
	qt.Assert(t, idx, qt.IsNotNil)
	qt.Assert(t, built, qt.IsTrue)
}

func TestEngine_AproposFindsUnloadedExports(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	// "vector-stable-sort" is exported only by (srfi 132), which has not
	// been imported. PrimApropos lazily builds the export index from the
	// file resolver and passes it to SearchDoc.
	result, err := eng.EvalMultiple(ctx, `(apropos "vector-stable-sort")`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Not(qt.Equals), "()",
		qt.Commentf("apropos should find vector-stable-sort in unloaded libraries"))
}
