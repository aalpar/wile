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
	c := qt.New(t)
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithSourceOS(), WithLibraryPaths("."))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	liveReg, ok := eng.Environment().Namespace().Registry().(*registry.Registry)
	c.Assert(ok, qt.IsTrue, qt.Commentf("namespace registry should be *registry.Registry"))

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
