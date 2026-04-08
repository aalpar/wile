package wile

import (
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/stdlib"
)

func TestDocRegistrationObserver_ImportUpdatesLiveRegistry(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithAllExtensions(), WithSourceFS(stdlib.FS), WithSourceOS(), WithLibraryPaths("."))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	liveReg := eng.Environment().Namespace().Registry().(*registry.Registry)

	// Before import: no algebra category.
	byCategory := liveReg.PrimitivesByCategory()
	_, hasAlgebra := byCategory["algebra"]
	qt.Assert(t, hasAlgebra, qt.IsFalse, qt.Commentf("algebra should not exist before import"))

	// Import algebra group library.
	_, err = eng.EvalMultiple(ctx, `(import (wile algebra group))`)
	qt.Assert(t, err, qt.IsNil)

	// Live registry should now have the algebra category.
	byCategory = liveReg.PrimitivesByCategory()
	algebraPrims, hasAlgebra := byCategory["algebra"]
	qt.Assert(t, hasAlgebra, qt.IsTrue, qt.Commentf("algebra should appear after import"))
	qt.Assert(t, len(algebraPrims) > 0, qt.IsTrue)

	// Verify specific procedures were registered.
	pr, found := liveReg.FindPrimitive("group-op", 0)
	qt.Assert(t, found, qt.IsTrue, qt.Commentf("group-op should be registered"))
	qt.Assert(t, pr.Spec.Category, qt.Equals, "algebra")

	// Verify the Scheme primitive (doc-topics) also sees it.
	result, err := eng.EvalMultiple(ctx, `(doc-topics)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, strings.Contains(result.SchemeString(), "algebra"), qt.IsTrue,
		qt.Commentf("(doc-topics) should contain algebra"))
}

func TestDocRegistrationObserver_CloneSnapshotBehavior(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithAllExtensions(), WithSourceFS(stdlib.FS), WithSourceOS(), WithLibraryPaths("."))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	// Clone taken before import misses new registrations.
	cloneBefore := eng.Registry()
	_, err = eng.EvalMultiple(ctx, `(import (wile algebra group))`)
	qt.Assert(t, err, qt.IsNil)

	byCategory := cloneBefore.PrimitivesByCategory()
	_, hasAlgebra := byCategory["algebra"]
	qt.Assert(t, hasAlgebra, qt.IsFalse,
		qt.Commentf("clone taken before import should NOT see algebra"))

	// Clone taken after import sees the registrations.
	cloneAfter := eng.Registry()
	byCategory = cloneAfter.PrimitivesByCategory()
	_, hasAlgebra = byCategory["algebra"]
	qt.Assert(t, hasAlgebra, qt.IsTrue,
		qt.Commentf("clone taken after import should see algebra"))
}
