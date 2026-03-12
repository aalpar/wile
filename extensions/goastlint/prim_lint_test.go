package goastlint_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extgoastlint "github.com/aalpar/wile/extensions/goastlint"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgoastlint.Extension),
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

func TestExtensionLibraryName(t *testing.T) {
	type libraryNamer interface {
		LibraryName() []string
	}
	namer, ok := extgoastlint.Extension.(libraryNamer)
	qt.New(t).Assert(ok, qt.IsTrue)
	qt.New(t).Assert(namer.LibraryName(), qt.DeepEquals, []string{"wile", "goast", "lint"})
}

func TestGoAnalyzeList_ReturnsStrings(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine, `(pair? (go-analyze-list))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoAnalyzeList_ContainsKnownAnalyzers(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	for _, name := range []string{"nilness", "shadow", "assign", "unreachable"} {
		result := runScheme(t, engine, `
			(let loop ((names (go-analyze-list)))
				(cond
					((null? names) #f)
					((equal? (car names) "`+name+`") #t)
					(else (loop (cdr names)))))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue,
			qt.Commentf("expected %q in go-analyze-list", name))
	}
}
