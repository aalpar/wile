package wile_test

import (
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
)

func TestEngine_FormLabel(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	// Built-in primitive
	carVal, ok := eng.Get("car")
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, eng.FormLabel(carVal), qt.Equals, "primitive")

	// User-defined procedure
	_, err = eng.EvalMultiple(ctx, `(define (f x) (+ x 1))`)
	qt.Assert(t, err, qt.IsNil)
	fVal, ok := eng.Get("f")
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, eng.FormLabel(fVal), qt.Equals, "procedure")

	// Non-callable
	qt.Assert(t, eng.FormLabel(wile.NewInteger(42)), qt.Equals, "")
}

func TestEngine_DisassembleValue_Native(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(define (add1 x) (+ x 1))`)
	qt.Assert(t, err, qt.IsNil)
	val, ok := eng.Get("add1")
	qt.Assert(t, ok, qt.IsTrue)

	dis, disErr := eng.DisassembleValue(val)
	qt.Assert(t, disErr, qt.IsNil)
	qt.Assert(t, strings.Contains(dis, "OP"), qt.IsTrue,
		qt.Commentf("disassembly should contain opcodes: %q", dis))
}

func TestEngine_DisassembleValue_Foreign(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	val, ok := eng.Get("car")
	qt.Assert(t, ok, qt.IsTrue)

	dis, disErr := eng.DisassembleValue(val)
	qt.Assert(t, disErr, qt.IsNil)
	qt.Assert(t, strings.Contains(dis, "foreign"), qt.IsTrue)
	qt.Assert(t, strings.Contains(dis, "car"), qt.IsTrue)
}

func TestEngine_DisassembleValue_NonProcedure(t *testing.T) {
	eng, err := wile.NewEngine(context.Background())
	qt.Assert(t, err, qt.IsNil)

	_, disErr := eng.DisassembleValue(wile.NewInteger(42))
	qt.Assert(t, disErr, qt.IsNotNil)
}
