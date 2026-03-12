package goastcfg_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extgoastcfg "github.com/aalpar/wile/extensions/goastcfg"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgoastcfg.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

func runScheme(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

func runSchemeExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNotNil)
}

func TestExtensionLibraryName(t *testing.T) {
	type libraryNamer interface {
		LibraryName() []string
	}
	namer, ok := extgoastcfg.Extension.(libraryNamer)
	qt.New(t).Assert(ok, qt.IsTrue)
	qt.New(t).Assert(namer.LibraryName(), qt.DeepEquals, []string{"wile", "goast", "cfg"})
}

func TestGoCFG_ReturnsCFGBlocks(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine,
		`(pair? (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFG_EntryBlockHasNoIdom(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine, `
		(let* ((blocks (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))
		       (entry  (car blocks)))
			(eq? (cdr (assoc 'idom (cdr entry))) #f))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFG_Errors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{name: "wrong pattern type", code: `(go-cfg 42 "Func")`},
		{name: "wrong func-name type", code: `(go-cfg "pkg" 42)`},
		{name: "nonexistent package", code: `(go-cfg "github.com/aalpar/wile/does-not-exist-xyz" "Foo")`},
		{name: "nonexistent function", code: `(go-cfg "github.com/aalpar/wile/extensions/goast" "NoSuchFunction")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeExpectError(t, engine, tc.code)
		})
	}
}
