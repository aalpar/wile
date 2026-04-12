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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/stdlib"
)

func TestEngine_LoadedLibraries_Empty(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)
	libs := eng.LoadedLibraries()
	qt.Assert(t, len(libs), qt.Equals, 0)
}

func TestEngine_LoadedLibraries_AfterImport(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(import (scheme base))`)
	qt.Assert(t, err, qt.IsNil)

	libs := eng.LoadedLibraries()
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
	info := eng.LookupLibrary("nonexistent", "lib")
	qt.Assert(t, info, qt.IsNil)
}

func TestEngine_LookupLibrary_Found(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(import (scheme base))`)
	qt.Assert(t, err, qt.IsNil)

	info := eng.LookupLibrary("scheme", "base")
	qt.Assert(t, info, qt.IsNotNil)
	qt.Assert(t, info.Name, qt.Equals, "(scheme base)")
}

func TestEngine_UnloadedLibraries(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
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
