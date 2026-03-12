// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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

func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
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

	result := eval(t, engine,
		`(pair? (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFG_EntryBlockHasNoIdom(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := eval(t, engine, `
		(let* ((blocks (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))
		       (entry  (car blocks)))
			(eq? (cdr (assoc 'idom (cdr entry))) #f))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFGDominators_Structure(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// A branching function guarantees multiple blocks with non-trivial dominance.
	eval(t, engine, `
		(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))`)

	result := eval(t, engine, `(pair? (go-cfg-dominators cfg))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Every dom-node must have block, idom, and children fields.
	result = eval(t, engine, `
		(let loop ((nodes (go-cfg-dominators cfg)))
			(if (null? nodes) #t
				(let ((n (car nodes)))
					(and (eq? (car n) 'dom-node)
					     (assoc 'block    (cdr n))
					     (assoc 'idom     (cdr n))
					     (assoc 'children (cdr n))
					     (loop (cdr nodes))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFGDominators_EntryDominatesAll(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Entry block (index 0) should appear in children of no other node
	// (it is the root — idom is #f).
	eval(t, engine,
		`(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))`)
	eval(t, engine,
		`(define dom (go-cfg-dominators cfg))`)
	// Walk dom-tree to find the entry block (the one with idom=#f)
	// and verify its block field is an integer.
	result := eval(t, engine, `
		(let loop ((nodes dom))
			(if (null? nodes) #f
				(let* ((n    (car nodes))
				       (idom (cdr (assoc 'idom (cdr n)))))
					(if (eq? #f idom)
						(integer? (cdr (assoc 'block (cdr n))))
						(loop (cdr nodes))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFGDominates(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	eval(t, engine,
		`(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))`)
	eval(t, engine,
		`(define dom (go-cfg-dominators cfg))`)

	// Entry block (0) dominates itself.
	result := eval(t, engine, `(go-cfg-dominates? dom 0 0)`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Entry block (0) dominates every other block.
	result = eval(t, engine, `
		(let loop ((nodes dom))
			(if (null? nodes) #t
				(let ((idx (cdr (assoc 'block (cdr (car nodes))))))
					(if (go-cfg-dominates? dom 0 idx)
						(loop (cdr nodes))
						#f))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFGPaths_LinearFunction(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// A linear function has at least one path from entry to last block.
	eval(t, engine,
		`(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoFormat"))`)

	result := eval(t, engine, `
		(let* ((last-idx (- (length cfg) 1))
		       (paths (go-cfg-paths cfg 0 last-idx)))
			(pair? paths))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFGPaths_BranchingFunction(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// A branching function has multiple paths; each path is a list of indices.
	eval(t, engine,
		`(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))`)

	result := eval(t, engine, `
		(let* ((last-idx (- (length cfg) 1))
		       (paths (go-cfg-paths cfg 0 last-idx)))
			(and (list? paths)
			     (if (pair? paths)
			         (list? (car paths))
			         #t)))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFGPaths_SameBlock(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	eval(t, engine,
		`(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoFormat"))`)

	// A path from block 0 to itself is a single path containing just block 0.
	result := eval(t, engine, `
		(let ((paths (go-cfg-paths cfg 0 0)))
			(and (pair? paths)
			     (equal? (car paths) '(0))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestIntegration_DominanceQuery(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Build CFG + dominator tree for a real function.
	eval(t, engine,
		`(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))`)
	eval(t, engine,
		`(define dom (go-cfg-dominators cfg))`)

	// Verify: entry block dominates every other block.
	result := eval(t, engine, `
		(let loop ((nodes dom))
			(if (null? nodes) #t
				(let ((idx (cdr (assoc 'block (cdr (car nodes))))))
					(and (go-cfg-dominates? dom 0 idx)
					     (loop (cdr nodes))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Verify: every block dominates itself.
	result = eval(t, engine, `
		(let loop ((blocks cfg))
			(if (null? blocks) #t
				(let* ((b     (car blocks))
				       (b-idx (cdr (assoc 'index (cdr b)))))
					(and (go-cfg-dominates? dom b-idx b-idx)
					     (loop (cdr blocks))))))`)
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
			evalExpectError(t, engine, tc.code)
		})
	}
}
