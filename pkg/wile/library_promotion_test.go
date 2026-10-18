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
// sites stay on the by-GlobalIndex load, as does a forward reference to a name
// defined later in the same library. Widening the gate to every library-scope
// reference would flip the first assertion.
func TestLibraryBodyKeepsGlobalLoadForNonImportedNames(t *testing.T) {
	ctx := context.Background()

	fsys := fstest.MapFS{
		"probe/noimport.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe noimport)
  (export ni-add)
  (begin (define (ni-add a b) (+ a b))))`),
		},
		"probe/fwd.sld": &fstest.MapFile{
			Data: []byte(`(define-library (probe fwd)
  (import (scheme base))
  (export fwd-first)
  (begin
    (define (fwd-first n) (fwd-second n))
    (define (fwd-second n) (+ n 1))))`),
		},
	}

	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(import (probe noimport) (probe fwd))`)
	qt.Assert(t, err, qt.IsNil)

	h := opHistogram(t, eng, "ni-add")
	qt.Assert(t, globalLoads(h) > 0, qt.IsTrue, qt.Commentf("no-import library histogram %v", h))

	got, err := eng.EvalMultiple(ctx, `(list (ni-add 2 3) (fwd-first 41))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, "(5 42)")
}
