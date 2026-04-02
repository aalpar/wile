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

package introspection_test

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// newAllEngine creates a Wile engine with all extensions enabled.
func newAllEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithAllExtensions(),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// alistLookup finds a key in an alist (list of (key . value) pairs).
// Returns the cdr of the matching pair and true, or nil and false.
func alistLookup(alist values.Tuple, key string) (values.Value, bool) {
	cur := values.Value(alist)
	for {
		pair, ok := cur.(*values.Pair)
		if !ok {
			return nil, false
		}
		entry, ok := pair.Car().(*values.Pair)
		if ok {
			sym, isSym := entry.Car().(*values.Symbol)
			if isSym && sym.Key == key {
				return entry.Cdr(), true
			}
		}
		cur = pair.Cdr()
	}
}

func TestPrimDisassemble_NativeClosure(t *testing.T) {
	c := qt.New(t)
	engine := newAllEngine(t)

	schemeEval(t, engine, `(define (add1 x) (+ x 1))`)
	result := schemeEval(t, engine, `(disassemble add1)`)

	inner := result.Internal()

	// Result is a pair (non-empty list).
	outerPair, ok := inner.(*values.Pair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected *Pair, got %T", inner))

	// First element is the header alist.
	header, ok := outerPair.Car().(values.Tuple)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected header to be Tuple, got %T", outerPair.Car()))

	// Header's first entry is (type . native-closure).
	typeVal, found := alistLookup(header, "type")
	c.Assert(found, qt.IsTrue, qt.Commentf("header missing 'type' key"))
	typeSym, ok := typeVal.(*values.Symbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected *Symbol for type value, got %T", typeVal))
	c.Assert(typeSym.Key, qt.Equals, "native-closure")
}

func TestPrimDisassemble_ForeignClosure(t *testing.T) {
	c := qt.New(t)
	engine := newAllEngine(t)

	result := schemeEval(t, engine, `(disassemble car)`)

	inner := result.Internal()

	// Result is a pair.
	outerPair, ok := inner.(*values.Pair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected *Pair, got %T", inner))

	// For foreign closures, the result itself is the alist.
	typeVal, found := alistLookup(outerPair, "type")
	c.Assert(found, qt.IsTrue, qt.Commentf("alist missing 'type' key"))
	typeSym, ok := typeVal.(*values.Symbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected *Symbol for type value, got %T", typeVal))
	c.Assert(typeSym.Key, qt.Equals, "foreign-closure")
}

func TestPrimDisassemble_CaseLambda(t *testing.T) {
	c := qt.New(t)
	engine := newAllEngine(t)

	schemeEval(t, engine, `(define f (case-lambda ((x) x) ((x y) (+ x y))))`)
	result := schemeEval(t, engine, `(disassemble f)`)

	inner := result.Internal()

	// Result is a pair.
	outerPair, ok := inner.(*values.Pair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected *Pair, got %T", inner))

	// For case-lambda, the result itself is the alist.
	typeVal, found := alistLookup(outerPair, "type")
	c.Assert(found, qt.IsTrue, qt.Commentf("alist missing 'type' key"))
	typeSym, ok := typeVal.(*values.Symbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected *Symbol for type value, got %T", typeVal))
	c.Assert(typeSym.Key, qt.Equals, "case-lambda-closure")
}

func TestPrimDisassemble_NotAProcedure(t *testing.T) {
	c := qt.New(t)
	engine := newAllEngine(t)

	expr, err := engine.Parse(context.Background(), `(disassemble 42)`)
	if err != nil {
		// Parse error is acceptable — the primitive still rejected non-procedures.
		c.Assert(strings.Contains(err.Error(), "disassemble"), qt.IsTrue)
		return
	}
	_, err = engine.Eval(context.Background(), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(strings.Contains(err.Error(), "disassemble"), qt.IsTrue,
		qt.Commentf("error message: %s", err.Error()))
}

func TestPrimDisassemble_InstructionKeys(t *testing.T) {
	c := qt.New(t)
	engine := newAllEngine(t)

	schemeEval(t, engine, `(define (id x) x)`)
	result := schemeEval(t, engine, `(disassemble id)`)

	inner := result.Internal()

	// Result is a list: (header instr1 instr2 ...).
	outerPair, ok := inner.(*values.Pair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected *Pair, got %T", inner))

	// Walk past header to the first instruction.
	tail, ok := outerPair.Cdr().(values.Tuple)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected cdr to be Tuple, got %T", outerPair.Cdr()))
	c.Assert(tail.IsEmptyList(), qt.IsFalse, qt.Commentf("expected at least one instruction after header"))

	firstInstr, ok := tail.Car().(values.Tuple)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected instruction to be Tuple, got %T", tail.Car()))

	// First instruction must have 'pc' and 'op' keys.
	_, foundPC := alistLookup(firstInstr, "pc")
	c.Assert(foundPC, qt.IsTrue, qt.Commentf("instruction missing 'pc' key"))

	_, foundOp := alistLookup(firstInstr, "op")
	c.Assert(foundOp, qt.IsTrue, qt.Commentf("instruction missing 'op' key"))
}
