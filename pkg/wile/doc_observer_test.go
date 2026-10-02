package wile

import (
	"context"
	"slices"
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/stdlib"
)

func TestDocRegistrationObserver_ImportUpdatesLiveRegistry(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithSourceOS(), WithLibraryPaths("."))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	liveReg, ok := eng.Environment().Namespace().Registry().(*registry.PrimitiveRegistry)
	c.Assert(ok, qt.IsTrue, qt.Commentf("namespace registry should be *registry.PrimitiveRegistry"))

	// Before import: no algebra category.
	byCategory := liveReg.PrimitivesByCategory()
	_, hasAlgebra := byCategory["algebra"]
	c.Assert(hasAlgebra, qt.IsFalse, qt.Commentf("algebra should not exist before import"))

	// Import algebra group library.
	_, err = eng.EvalMultiple(ctx, `(import (wile algebra group))`)
	c.Assert(err, qt.IsNil)

	// Live registry should now have the algebra category.
	byCategory = liveReg.PrimitivesByCategory()
	algebraPrims, hasAlgebra := byCategory["algebra"]
	c.Assert(hasAlgebra, qt.IsTrue, qt.Commentf("algebra should appear after import"))
	c.Assert(len(algebraPrims) > 0, qt.IsTrue)

	// Verify specific procedures were registered.
	pr, found := liveReg.FindPrimitive("group-op", 0)
	c.Assert(found, qt.IsTrue, qt.Commentf("group-op should be registered"))
	c.Assert(pr.Spec.Category, qt.Equals, "algebra")

	// Verify the Scheme primitive (doc-topics) also sees it.
	result, err := eng.EvalMultiple(ctx, `(doc-topics)`)
	c.Assert(err, qt.IsNil)
	c.Assert(strings.Contains(result.SchemeString(), "algebra"), qt.IsTrue,
		qt.Commentf("(doc-topics) should contain algebra"))
}

func TestDocRegistrationObserver_CloneSnapshotBehavior(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithSourceOS(), WithLibraryPaths("."))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// Clone taken before import misses new registrations.
	cloneBefore := eng.Registry()
	_, err = eng.EvalMultiple(ctx, `(import (wile algebra group))`)
	c.Assert(err, qt.IsNil)

	byCategory := cloneBefore.PrimitivesByCategory()
	_, hasAlgebra := byCategory["algebra"]
	c.Assert(hasAlgebra, qt.IsFalse,
		qt.Commentf("clone taken before import should NOT see algebra"))

	// Clone taken after import sees the registrations.
	cloneAfter := eng.Registry()
	byCategory = cloneAfter.PrimitivesByCategory()
	_, hasAlgebra = byCategory["algebra"]
	c.Assert(hasAlgebra, qt.IsTrue,
		qt.Commentf("clone taken after import should see algebra"))
}

// doclibFS is the fixture for the import-shape table below. The for-syntax
// export is written as `define-for-syntax` inside a `(begin ...)` declaration
// DELIBERATELY: `begin-for-syntax` is not a library declaration in Wile
// (`unknown library declaration: begin-for-syntax`), so this is the only shape
// that validates. A `define-library` form evaluated at the top level also does
// NOT register the library for a later `(import)`, which is why this is a file
// in a source FS rather than a string handed to EvalMultiple.
func doclibFS() fstest.MapFS {
	return fstest.MapFS{
		"doclib.scm": &fstest.MapFile{Data: []byte(`(define-library (doclib)
  (import (scheme base))
  (export plainproc (for-syntax fsproc))
  (begin
    (define (plainproc x)
      "Plain procedure.

Category: doclib-demo
Signature: (plainproc x)
"
      x)
    (define-for-syntax (fsproc y)
      "For-syntax procedure.

Category: doclib-demo
Signature: (fsproc y)
"
      y)))
`)},
	}
}

// A ,topic listing names every import shape's LOCAL names, at every exported
// phase. I023.
//
// The observer looped over evt.Imported, which is the external names with the
// phase dropped and the local name lost, so it failed twice over and silently:
// a for-syntax export was looked up at phase 0 where it is not bound, and a
// renamed or prefixed import was looked up under a name the library never
// exported, finding nothing at all.
//
// SIX CELLS, three import shapes by two exports, and all six were red — the
// three rename/prefix cells by registering NOTHING, the three for-syntax cells
// by phase. Measured on master: plain reports "1 procedures", and both rename
// and prefix report `No category "doclib-demo"`.
//
// The cells assert NAMES, never a count: the count is a property of the fixture
// (on a 94-export library the rename row would report 93 rather than 0) and
// would pass for the wrong reason the moment the fixture changed.
func TestDocRegistrationObserver_EveryImportShapeRegistersLocalNames(t *testing.T) {
	for _, row := range []struct {
		name string
		src  string
		want []string
	}{
		{"plain", `(import (doclib))`, []string{"fsproc", "plainproc"}},
		{"rename", `(import (rename (doclib) (plainproc pp)))`, []string{"fsproc", "pp"}},
		{"prefix", `(import (prefix (doclib) z:))`, []string{"z:fsproc", "z:plainproc"}},
	} {
		t.Run(row.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			eng, err := NewEngine(ctx, WithProfile(KitchenSink),
				WithSourceFS(doclibFS()), WithSourceFS(stdlib.FS), WithLibraryPaths("."))
			c.Assert(err, qt.IsNil)
			defer eng.Close()

			liveReg, ok := eng.Environment().Namespace().Registry().(*registry.PrimitiveRegistry)
			c.Assert(ok, qt.IsTrue)

			_, err = eng.EvalMultiple(ctx, row.src)
			c.Assert(err, qt.IsNil)

			var got []string
			for _, pr := range liveReg.PrimitivesByCategory()["doclib-demo"] {
				got = append(got, pr.Spec.Name)
			}
			slices.Sort(got)
			c.Assert(got, qt.DeepEquals, row.want)
		})
	}
}

// LibraryImportEvent.Bindings is a SNAPSHOT. The observer runs before
// CopyLibraryBindingsToEnvAtPhase at both call sites, so publishing the
// installer's own map would let a third-party observer rewrite the import
// synchronously. This pin fails if the maps.Clone is ever dropped: it deletes
// every entry from the map it is handed and then checks that the import
// installed anyway.
func TestDocRegistrationObserver_BindingsIsASnapshot(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	cleared := 0
	eng, err := NewEngine(ctx, WithProfile(KitchenSink),
		WithSourceFS(doclibFS()), WithSourceFS(stdlib.FS), WithLibraryPaths("."),
		WithImportObserver(func(evt LibraryImportEvent) {
			if evt.Library.Key() != "doclib" {
				return
			}
			cleared += len(evt.Bindings)
			for k := range evt.Bindings {
				delete(evt.Bindings, k)
			}
		}))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	v, err := eng.EvalMultiple(ctx, `(import (doclib)) (plainproc 7)`)
	c.Assert(err, qt.IsNil, qt.Commentf("an observer that empties its map must not break the import"))
	c.Assert(v.SchemeString(), qt.Equals, "7")
	c.Assert(cleared > 0, qt.IsTrue, qt.Commentf("precondition: the observer saw bindings to clear"))
}
