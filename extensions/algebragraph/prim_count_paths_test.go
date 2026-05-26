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

package algebragraph_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/extensions/algebragraph"
)

// newEngine builds a Wile engine with only the algebragraph extension.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(algebragraph.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// evalString evaluates Scheme source and returns its string representation
// via Wile's external-printable form. This avoids per-test type assertions
// for vectors of bignums, which are the dominant output shape here.
func evalString(t *testing.T, engine *wile.Engine, code string) string {
	t.Helper()
	result, err := engine.EvalMultiple(context.Background(), code)
	qt.Assert(t, err, qt.IsNil)
	return result.SchemeString()
}

// evalExpectError asserts the Scheme code raises a Go-side error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.Assert(t, err, qt.IsNotNil)
}

// --- count-paths-in-dag ---

func TestCountPathsInDAG_SingleNode(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// One node, no edges: count is #(1).
	c.Assert(evalString(t, engine, `(count-paths-in-dag 1 '() 0)`),
		qt.Equals, "#(1)")
}

func TestCountPathsInDAG_LinearChain(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// 0 → 1 → 2 → 3, source 0: one path to each node.
	c.Assert(evalString(t, engine, `(count-paths-in-dag 4 '((0 . 1) (1 . 2) (2 . 3)) 0)`),
		qt.Equals, "#(1 1 1 1)")
}

func TestCountPathsInDAG_Diamond(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// Diamond: A=0 → {1,2} → 3. Two paths to node 3.
	c.Assert(evalString(t, engine, `(count-paths-in-dag 4 '((0 . 1) (0 . 2) (1 . 3) (2 . 3)) 0)`),
		qt.Equals, "#(1 1 1 2)")
}

func TestCountPathsInDAG_CyclicReturnsFalse(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// 0 → 1 → 0 (cycle reachable from source). Primitive returns #f.
	c.Assert(evalString(t, engine, `(count-paths-in-dag 2 '((0 . 1) (1 . 0)) 0)`),
		qt.Equals, "#f")
}

func TestCountPathsInDAG_SelfLoopReturnsFalse(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// 0 → 0 is a cycle from source.
	c.Assert(evalString(t, engine, `(count-paths-in-dag 1 '((0 . 0)) 0)`),
		qt.Equals, "#f")
}

func TestCountPathsInDAG_ErrorOnInvalidInput(t *testing.T) {
	engine := newEngine(t)
	// num-nodes not an integer.
	evalExpectError(t, engine, `(count-paths-in-dag "two" '() 0)`)
	// edges not a list.
	evalExpectError(t, engine, `(count-paths-in-dag 2 42 0)`)
	// edge cdr not an integer.
	evalExpectError(t, engine, `(count-paths-in-dag 2 '((0 . "x")) 0)`)
	// source out of range.
	evalExpectError(t, engine, `(count-paths-in-dag 2 '() 5)`)
	// negative num-nodes.
	evalExpectError(t, engine, `(count-paths-in-dag -1 '() 0)`)
}

// --- count-paths-cyclic ---

func TestCountPathsCyclic_AcyclicDiamond(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// Diamond with source 0. Acyclic: each node is its own SCC.
	// Result is three values (scc-vec, counts-vec, nontrivial-vec).
	// The diamond's interior nodes get arbitrary IDs but SCC[0] = 0
	// and SCC[3] = 3 are forced; per-node count for node 3 is 2.
	code := `
(call-with-values
  (lambda () (count-paths-cyclic 4 '((0 . 1) (0 . 2) (1 . 3) (2 . 3)) 0))
  (lambda (scc counts nt)
    (list (vector-ref scc 0)
          (vector-ref scc 3)
          (vector-ref counts (vector-ref scc 0))
          (vector-ref counts (vector-ref scc 3))
          (vector-ref nt (vector-ref scc 0))
          (vector-ref nt (vector-ref scc 3)))))`
	c.Assert(evalString(t, engine, code), qt.Equals, "(0 3 1 2 #f #f)")
}

func TestCountPathsCyclic_SingleCycle(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// 0 → 1 → 2 → 0 — all three nodes in one non-trivial SCC.
	// Source's SCC has count 1.
	code := `
(call-with-values
  (lambda () (count-paths-cyclic 3 '((0 . 1) (1 . 2) (2 . 0)) 0))
  (lambda (scc counts nt)
    (list (vector-length counts)
          (vector-ref scc 0)
          (vector-ref scc 1)
          (vector-ref scc 2)
          (vector-ref counts 0)
          (vector-ref nt 0))))`
	c.Assert(evalString(t, engine, code), qt.Equals, "(1 0 0 0 1 #t)")
}

func TestCountPathsCyclic_CycleWithTail(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// Cycle {0,1,2} plus tail 0→3. Source 0 in cycle SCC.
	// cycle SCC = 0 (low ID — source), tail SCC = 1.
	// Counts: cycle = 1, tail = 1.
	// NonTrivial: cycle = #t, tail = #f.
	code := `
(call-with-values
  (lambda () (count-paths-cyclic 4 '((0 . 1) (1 . 2) (2 . 0) (0 . 3)) 0))
  (lambda (scc counts nt)
    (list (vector-ref scc 0)
          (vector-ref scc 3)
          (vector-ref counts (vector-ref scc 0))
          (vector-ref counts (vector-ref scc 3))
          (vector-ref nt (vector-ref scc 0))
          (vector-ref nt (vector-ref scc 3)))))`
	c.Assert(evalString(t, engine, code), qt.Equals, "(0 1 1 1 #t #f)")
}

func TestCountPathsCyclic_MutualRecursionWithMultiEdge(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// main → f, main → g, f → helper × 2, f → g, g → f, g → helper.
	// SCCs: {main}, {f, g} (non-trivial), {helper}.
	// Entry count to {f, g}: 2 (two main→{f,g} edges).
	// Count to {helper}: 2 × 3 = 6 (entry-count × inter-SCC out-edges).
	code := `
(call-with-values
  (lambda () (count-paths-cyclic 4
                '((0 . 1) (0 . 2) (1 . 3) (1 . 3) (1 . 2) (2 . 1) (2 . 3))
                0))
  (lambda (scc counts nt)
    (list (vector-ref counts (vector-ref scc 0))
          (vector-ref counts (vector-ref scc 1))
          (vector-ref counts (vector-ref scc 3))
          (vector-ref nt (vector-ref scc 1)))))`
	c.Assert(evalString(t, engine, code), qt.Equals, "(1 2 6 #t)")
}

func TestCountPathsCyclic_ErrorOnInvalidInput(t *testing.T) {
	engine := newEngine(t)
	evalExpectError(t, engine, `(count-paths-cyclic "two" '() 0)`)
	evalExpectError(t, engine, `(count-paths-cyclic 2 42 0)`)
	evalExpectError(t, engine, `(count-paths-cyclic 2 '() 5)`)
}
