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

package wile

import (
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
)

// TestPromoteCompoundArgsFib is the end-to-end guard for the pass-4 peephole
// fusion (fusePromotedCompoundArgs): a tail call to a promoted primitive whose
// arguments are compound subexpressions is promoted to the inline op. fib's
// (+ (fib (- n 1)) (fib (- n 2))) is the canonical case — its `+` previously
// stayed on the generic apply path while `-` and `<=` were inlined.
//
// The test pins three things: the recursion still computes correctly (no
// miscompile), variadic / non-tail `+` are untouched (the fusion is strictly
// 2-ary and tail-only), and the promotion actually fires (OpAddTail appears in
// the compiled bytecode — guarding against a dead optimization).
func TestPromoteCompoundArgsFib(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithLibraryPaths())
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, "(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))")
	c.Assert(err, qt.IsNil)

	// Correctness: the promoted tail `+` must still compute fib (0-indexed).
	res, err := eng.EvalMultiple(ctx, "(fib 20)")
	c.Assert(err, qt.IsNil)
	c.Assert(res.SchemeString(), qt.Equals, "6765")

	// Variadic `+` (≠ 2 args) is not 2-ary-promotable and must still fold.
	res, err = eng.EvalMultiple(ctx, "(+ 1 2 3 4)")
	c.Assert(err, qt.IsNil)
	c.Assert(res.SchemeString(), qt.Equals, "10")

	// Non-tail `+` with compound args stays on the generic path and still works:
	// (* 2 (+ (fib 5) (fib 6))) = 2 * (5 + 8) = 26.
	res, err = eng.EvalMultiple(ctx, "(* 2 (+ (fib 5) (fib 6)))")
	c.Assert(err, qt.IsNil)
	c.Assert(res.SchemeString(), qt.Equals, "26")

	// Fires: fib's tail `+` is promoted to OpAddTail (renders as "AddTail").
	// fib has exactly one `+`, in tail position, so this is unambiguous.
	fibVal, err := eng.EvalMultiple(ctx, "fib")
	c.Assert(err, qt.IsNil)
	dis, err := eng.DisassembleValue(fibVal)
	c.Assert(err, qt.IsNil)
	c.Assert(strings.Contains(dis, "AddTail"), qt.IsTrue,
		qt.Commentf("fib's tail + should promote to OpAddTail; disassembly:\n%s", dis))
}
