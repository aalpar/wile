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

package wile_test

import (
	"context"
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// opHistogram counts the opcodes of a procedure's own template by the
// disassembler's spelling.
func opHistogram(t *testing.T, eng *wile.Engine, name string) map[string]int {
	t.Helper()

	v, ok := eng.Get(name)
	qt.Assert(t, ok, qt.IsTrue)
	closure, ok := v.Internal().(*machine.MachineClosure)
	qt.Assert(t, ok, qt.IsTrue)

	q := map[string]int{}
	for _, in := range machine.Disassemble(closure.Template()).Instructions {
		q[in.Op]++
	}
	return q
}

// globalLoads counts the template's by-GlobalIndex loads. The disassembler
// names the emitted OpLoadGlobal "PushGlobal" once the peephole has promoted
// it, so both spellings count.
func globalLoads(h map[string]int) int {
	n := 0
	for op, c := range h {
		if strings.Contains(op, "Global") {
			n += c
		}
	}
	return n
}

// TestLibraryBodyPromotesImportedPrimitiveLoads pins I217: an imported
// primitive referenced in a library body compiles to the same cached-binding
// load it gets at top level, so the peephole promotes + - < to AddTail, Sub and
// NumLt. A value assertion cannot see the defect (both shapes answer 196418),
// so the gate counts opcodes.
//
// The opcode-count assertions are a RATCHET on a deliberate non-optimization:
// they pin that library code reaches the top-level shape, not that widening
// the gate to non-imported names is unsafe (it is not shown to be).
func TestLibraryBodyPromotesImportedPrimitiveLoads(t *testing.T) {
	ctx := context.Background()

	fib := `(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))`
	// The library procedure has its own name so the import cannot collide with
	// the top-level one the oracle reads.
	libFib := strings.ReplaceAll(fib, "fib", "lib-fib")
	fsys := fstest.MapFS{
		"probe/fibl.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe fibl)
  (import (scheme base))
  (export lib-fib)
  (begin ` + libFib + `))`),
		},
	}

	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(import (scheme base)) `+fib+` (define top-fib fib)`)
	qt.Assert(t, err, qt.IsNil)
	topHist := opHistogram(t, eng, "top-fib")

	_, err = eng.EvalMultiple(ctx, `(import (probe fibl))`)
	qt.Assert(t, err, qt.IsNil)
	libHist := opHistogram(t, eng, "lib-fib")

	// The top-level shape is the oracle for what promotion looks like.
	qt.Assert(t, topHist["AddTail"] > 0, qt.IsTrue, qt.Commentf("top-level histogram %v", topHist))

	qt.Assert(t, globalLoads(libHist), qt.Equals, 0, qt.Commentf("library histogram %v", libHist))
	for _, op := range []string{"AddTail", "NumLt", "Sub"} {
		qt.Assert(t, libHist[op] > 0, qt.IsTrue, qt.Commentf("library has no %s: %v", op, libHist))
	}
}

// TestLibraryBodyKeepsGlobalLoadForNonImportedNames pins the gate's width: only
// an imported binding is promoted. A library with no import clause gets its
// primitives from applyBaseEnvironment's sealed base (imported=false), and those
// sites stay on the by-GlobalIndex load. Widening the gate to every library-scope
// reference would flip the first assertion. (The nil-binding fallback for a name
// not yet defined is defensive and untested: no site in the suite reaches it.)
func TestLibraryBodyKeepsGlobalLoadForNonImportedNames(t *testing.T) {
	ctx := context.Background()

	fsys := fstest.MapFS{
		"probe/noimport.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe noimport)
  (export ni-add)
  (begin (define (ni-add a b) (+ a b))))`),
		},
	}

	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(import (probe noimport))`)
	qt.Assert(t, err, qt.IsNil)

	h := opHistogram(t, eng, "ni-add")
	qt.Assert(t, globalLoads(h) > 0, qt.IsTrue, qt.Commentf("no-import library histogram %v", h))

	got, err := eng.EvalMultiple(ctx, `(ni-add 2 3)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, "5")
}

// TestLibraryBodyCachedLoadSeesTheExportersSet pins that the cached-binding load
// is late: it reads the binding's value when it runs, not when it compiles. An
// importing library reads a variable its exporter later assigns, so a load that
// snapshotted the value at compile time (a literal) answers 0 twice.
func TestLibraryBodyCachedLoadSeesTheExportersSet(t *testing.T) {
	ctx := context.Background()

	fsys := fstest.MapFS{
		"probe/cnt.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe cnt)
  (import (scheme base))
  (export counter bump!)
  (begin
    (define counter 0)
    (define (bump!) (set! counter (+ counter 1)))))`),
		},
		"probe/usecnt.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe usecnt)
  (import (scheme base) (probe cnt))
  (export read-counter)
  (begin (define (read-counter) counter)))`),
		},
	}

	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	got, err := eng.EvalMultiple(ctx, `(import (scheme base) (probe cnt) (probe usecnt))
(let* ((a (read-counter)) (x (bump!)) (y (bump!)) (b (read-counter))) (list a b))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, "(0 2)")

	h := opHistogram(t, eng, "read-counter")
	loads := 0
	for op, c := range h {
		if strings.Contains(op, "CachedBinding") {
			loads += c
		}
	}
	qt.Assert(t, loads, qt.Equals, 1, qt.Commentf("read-counter histogram %v", h))
}
