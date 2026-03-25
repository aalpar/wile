package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/stdlib"
)

func TestEngine_EmbeddedStdlib_SchemeBase(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (scheme base))
		(+ 1 2)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")
}

func TestEngine_EmbeddedStdlib_ChibiTest(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	_, err = eng.EvalMultiple(ctx, `
		(import (chibi test))
		(test-begin "embedded")
		(test-assert (= 1 1))
		(test-end)
	`)
	c.Assert(err, qt.IsNil)
}

func TestEngine_EmbeddedStdlib_SRFI1(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (srfi 1))
		(fold + 0 '(1 2 3 4 5))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "15")
}

func TestEngine_EmbeddedStdlib_WileAlgebra(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra group))
		(group? (make-group + - 0))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")
}
