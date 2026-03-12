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

func runSchemeExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNotNil)
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

func TestGoAnalyze_ReturnsListForKnownPackage(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Run a simple analyzer on a known package.
	// Result may be empty (no issues) or non-empty — both are valid.
	result := runScheme(t, engine,
		`(list? (go-analyze "github.com/aalpar/wile/extensions/goast" "assign"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoAnalyze_DiagnosticStructure(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// If any diagnostics are returned, verify they have expected fields.
	result := runScheme(t, engine, `
		(let ((diags (go-analyze "github.com/aalpar/wile/extensions/goast" "assign")))
			(if (null? diags) #t
				(let ((d (car diags)))
					(and (eq? (car d) 'diagnostic)
					     (string? (cdr (assoc 'analyzer (cdr d))))
					     (string? (cdr (assoc 'pos      (cdr d))))
					     (string? (cdr (assoc 'message  (cdr d))))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoAnalyze_MultipleAnalyzers(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine,
		`(list? (go-analyze "github.com/aalpar/wile/extensions/goast" "assign" "unreachable"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoAnalyze_Errors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{name: "wrong pattern type", code: `(go-analyze 42 "assign")`},
		{name: "unknown analyzer name", code: `(go-analyze "github.com/aalpar/wile/extensions/goast" "no-such-analyzer")`},
		{name: "nonexistent package", code: `(go-analyze "github.com/aalpar/wile/does-not-exist-xyz" "assign")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeExpectError(t, engine, tc.code)
		})
	}
}

func TestIntegration_AnalyzeRealPackage(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Run multiple analyzers at once on the goastlint package itself.
	// Every diagnostic (if any) must have correct structure.
	result := runScheme(t, engine, `
		(let* ((diags (go-analyze "github.com/aalpar/wile/extensions/goastlint"
		                          "assign" "unreachable" "structtag"))
		       (nf (lambda (node key)
		             (let ((e (assoc key (cdr node))))
		               (if e (cdr e) #f)))))
		  (let loop ((ds diags))
		    (if (null? ds) #t
		      (let ((d (car ds)))
		        (and (eq? (car d) 'diagnostic)
		             (string? (nf d 'analyzer))
		             (string? (nf d 'pos))
		             (string? (nf d 'message))
		             (loop (cdr ds)))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestIntegration_AllAnalyzersRunnable(t *testing.T) {
	// Verify that every registered analyzer can run without panicking.
	// Run go-analyze-list and run each analyzer on a simple, known package.
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine, `
		(let loop ((ns (go-analyze-list)) (ok #t))
		  (if (null? ns) ok
		    (let ((diags (go-analyze "github.com/aalpar/wile/extensions/goast"
		                             (car ns))))
		      (loop (cdr ns) (and ok (list? diags))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}
