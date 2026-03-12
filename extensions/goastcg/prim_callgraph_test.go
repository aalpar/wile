package goastcg_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extgoastcg "github.com/aalpar/wile/extensions/goastcg"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgoastcg.Extension),
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
	namer, ok := extgoastcg.Extension.(libraryNamer)
	qt.New(t).Assert(ok, qt.IsTrue)
	qt.New(t).Assert(namer.LibraryName(), qt.DeepEquals, []string{"wile", "goast", "callgraph"})
}

func TestGoCallgraph_Static(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Load a known package with static analysis.
	result := runScheme(t, engine,
		`(pair? (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraph_CHA(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine,
		`(pair? (go-callgraph "github.com/aalpar/wile/extensions/goast" 'cha))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraph_Errors(t *testing.T) {
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
	}{
		{name: "wrong pattern type", code: `(go-callgraph 42 'static)`},
		{name: "wrong algorithm type", code: `(go-callgraph "pkg" "static")`},
		{name: "invalid algorithm", code: `(go-callgraph "github.com/aalpar/wile/extensions/goast" 'unknown)`},
		{name: "nonexistent package", code: `(go-callgraph "github.com/aalpar/wile/does-not-exist-xyz" 'static)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeExpectError(t, engine, tc.code)
		})
	}
}

// goastTestFunc is the fully-qualified SSA name for PrimGoParseExpr in the goast package.
// ssa.Function.String() returns the full module path, not the short package alias.
const goastTestFunc = "github.com/aalpar/wile/extensions/goast.PrimGoParseExpr"

func TestGoCallgraphCallers(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Build a static callgraph for a package.
	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// Returns a list (may be empty for a function not called by other goast functions).
	result := runScheme(t, engine,
		`(list? (go-callgraph-callers cg "`+goastTestFunc+`"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraphCallees(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// PrimGoCallgraph calls helpers and security — it should have outgoing edges.
	result := runScheme(t, engine,
		`(pair? (go-callgraph-callees cg "`+goastTestFunc+`"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraphCallers_NotFound(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// Nonexistent function returns empty list.
	result := runScheme(t, engine,
		`(null? (go-callgraph-callers cg "does.not.Exist"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestMapCallgraph_Reachable(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// Reachable from a known function should return a non-empty list of strings.
	result := runScheme(t, engine,
		`(pair? (go-callgraph-reachable cg "`+goastTestFunc+`"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// The root itself should appear in the reachable set.
	result = runScheme(t, engine, `
		(let ((reachable (go-callgraph-reachable cg "`+goastTestFunc+`")))
			(let loop ((r reachable))
				(cond
					((null? r) #f)
					((equal? (car r) "`+goastTestFunc+`") #t)
					(else (loop (cdr r))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraphReachable_NotFound(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// Nonexistent root returns empty list.
	result := runScheme(t, engine,
		`(null? (go-callgraph-reachable cg "does.not.Exist"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestIntegration_CallgraphQuery(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Build static callgraph for the goast extension.
	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// Define a helper to extract a named field from an alist node.
	runScheme(t, engine, `
		(define (nf node key)
			(let ((e (assoc key (cdr node))))
				(if e (cdr e) #f)))`)

	// Verify the graph has cg-node entries with expected structure.
	result := runScheme(t, engine, `
		(let ((first-node (car cg)))
			(and (eq? (car first-node) 'cg-node)
			     (string? (nf first-node 'name))
			     (integer? (nf first-node 'id))
			     (list? (nf first-node 'edges-in))
			     (list? (nf first-node 'edges-out))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Verify edges have expected structure.
	result = runScheme(t, engine, `
		(let* ((node (car cg))
		       (edges (nf node 'edges-out)))
			(if (null? edges)
				;; Skip if this node has no outgoing edges.
				#t
				(let ((edge (car edges)))
					(and (eq? (car edge) 'cg-edge)
					     (string? (nf edge 'description))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Verify reachable returns a list of strings.
	result = runScheme(t, engine, `
		(let ((reachable (go-callgraph-reachable cg "`+goastTestFunc+`")))
			(if (null? reachable)
				#t
				(string? (car reachable))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraph_RTA_NoMain(t *testing.T) {
	// RTA on a library package (no main) should error.
	engine := newEngine(t)
	runSchemeExpectError(t, engine,
		`(go-callgraph "github.com/aalpar/wile/extensions/goast" 'rta)`)
}
