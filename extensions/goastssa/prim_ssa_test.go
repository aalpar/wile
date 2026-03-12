package goastssa_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extgoastssa "github.com/aalpar/wile/extensions/goastssa"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgoastssa.Extension),
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
	namer, ok := extgoastssa.Extension.(libraryNamer)
	qt.New(t).Assert(ok, qt.IsTrue)
	qt.New(t).Assert(namer.LibraryName(), qt.DeepEquals, []string{"wile", "goast", "ssa"})
}

func TestGoSSABuild_WithPositionsOption(t *testing.T) {
	engine := newEngine(t)
	// The positions option is accepted without error.
	result := runScheme(t, engine,
		`(pair? (go-ssa-build "github.com/aalpar/wile/extensions/goast" 'positions))`)
	qt.New(t).Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoSSABuild_ReturnsListOfFunctions(t *testing.T) {
	engine := newEngine(t)

	// Load a known package, verify we get a list back.
	result := runScheme(t, engine,
		`(pair? (go-ssa-build "github.com/aalpar/wile/extensions/goast"))`)
	qt.New(t).Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoSSABuild_FunctionStructure(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Cache the SSA build result.
	runScheme(t, engine, `(define funcs (go-ssa-build "github.com/aalpar/wile/extensions/goast"))`)

	t.Run("function has name", func(t *testing.T) {
		// Find a known function: PrimGoParseExpr
		result := runScheme(t, engine, `
			(let loop ((fs funcs))
				(cond
					((null? fs) #f)
					((equal? (cdr (assoc 'name (cdr (car fs)))) "PrimGoParseExpr")
					 (car fs))
					(else (loop (cdr fs)))))`)
		// Should find the function (not #f).
		c.Assert(result.Internal(), qt.Not(qt.Equals), values.FalseValue)
	})

	t.Run("function has params", func(t *testing.T) {
		result2 := runScheme(t, engine, `
			(let ((fn (car funcs)))
				(assoc 'params (cdr fn)))`)
		c.Assert(result2.Internal(), qt.Not(qt.Equals), values.FalseValue)
	})

	t.Run("function has blocks", func(t *testing.T) {
		result := runScheme(t, engine, `
			(let ((fn (car funcs)))
				(assoc 'blocks (cdr fn)))`)
		c.Assert(result.Internal(), qt.Not(qt.Equals), values.FalseValue)
	})

	t.Run("block has index", func(t *testing.T) {
		result := runScheme(t, engine, `
			(let* ((fn (car funcs))
				   (blocks (cdr (assoc 'blocks (cdr fn))))
				   (block (car blocks)))
				(cdr (assoc 'index (cdr block))))`)
		c.Assert(result.Internal(), qt.Equals, values.NewInteger(0))
	})
}

func TestIntegration_FieldStoreQuery(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Define helpers and build SSA for a package with known struct field accesses.
	runScheme(t, engine, `(define funcs (go-ssa-build "github.com/aalpar/wile/extensions/goast"))`)
	runScheme(t, engine, `
		(define (tag? node t)
			(and (pair? node) (eq? (car node) t)))`)
	runScheme(t, engine, `
		(define (walk-instrs fn pred)
			(let loop ((blocks (cdr (assoc 'blocks (cdr fn)))) (acc '()))
				(if (null? blocks) (reverse acc)
					(let ((instrs (cdr (assoc 'instrs (cdr (car blocks))))))
						(loop (cdr blocks)
							(let iloop ((is instrs) (a acc))
								(if (null? is) a
									(iloop (cdr is)
										(if (pred (car is))
											(cons (car is) a)
											a)))))))))`)

	// Query: do any functions contain ssa-field-addr instructions?
	// The goast package has struct field accesses (mapperOpts, etc.)
	// so we expect at least one.
	result := runScheme(t, engine, `
		(let loop ((fs funcs))
			(if (null? fs) #f
				(let ((addrs (walk-instrs (car fs) (lambda (i) (tag? i 'ssa-field-addr)))))
					(if (pair? addrs) #t (loop (cdr fs))))))`)

	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoSSABuild_Errors(t *testing.T) {
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
	}{
		{name: "wrong arg type", code: `(go-ssa-build 42)`},
		{name: "nonexistent package", code: `(go-ssa-build "github.com/aalpar/wile/does-not-exist-xyz")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeExpectError(t, engine, tc.code)
		})
	}
}
