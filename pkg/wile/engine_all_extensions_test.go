package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

func TestKitchenSink_EngineCreation(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// Verify a primitive from each extension category exists.
	for _, name := range []string{
		"display",          // io
		"open-input-file",  // files
		"sin",              // math
		"procedure-arity",  // introspection
		"eval",             // eval
		"make-thread",      // threads
		"make-rw-mutex",    // gointerop
		"make-record-type", // all
		"command-line",     // system
	} {
		_, found := eng.Get(name)
		c.Assert(found, qt.IsTrue, qt.Commentf("missing: %s", name))
	}
}

func TestKitchenSink_Smoke(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// Smoke test: evaluate a simple expression using a non-core primitive.
	result, err := eng.Eval(ctx, eng.MustParse(ctx, "(sin 0)"))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "0.0")
}
