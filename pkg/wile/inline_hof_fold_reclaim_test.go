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
	"fmt"
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
)

// foldReclaimSetup imports srfi/1 (fold is import-gated, so the import is what
// stamps its InlineHOF capability), then builds a list of n cached small integers
// and a capture-safe two-argument kons that returns the accumulator unchanged
// (no per-element allocation, so the env-frame reclaim shows in an absolute
// allocation slope). The measured run is only the (fold cb 0 lst) call.
func foldReclaimSetup(n int) string {
	return fmt.Sprintf(`(begin
(import (srfi 1))
(define (cb x acc) acc)
(define (build i acc) (if (= i 0) acc (build (- i 1) (cons i acc))))
(define lst (build %d '())))`, n)
}

// TestInlineHOFFoldReclaims is the P6 reclaim gate for fold (the arity-3 list
// fold). (fold <capture-safe-kons> knil lst) must inline fold's single-list loop
// — the (null? lists) then-branch of srfi/1/fold.scm — so the loop self-tail-
// reclaims its env frame instead of leaking ~2 frames/element. kons runs in
// non-tail position (an argument to lp); only (lp (cdr ls) ...) is the self-tail
// call. Absolute allocation slope across two list sizes: ~0 when reclaimed.
func TestInlineHOFFoldReclaims(t *testing.T) {
	a1 := allocsForRun(t, foldReclaimSetup(1000), "(fold cb 0 lst)")
	a2 := allocsForRun(t, foldReclaimSetup(2000), "(fold cb 0 lst)")
	slope := (a2 - a1) / 1000.0
	if slope > 0.5 {
		t.Errorf("fold does not reclaim: %.3f allocs/element (a1=%.0f@1000, a2=%.0f@2000); "+
			"want ~0 — the inlined single-list loop should self-tail-reclaim", slope, a1, a2)
	}
}

// TestInlineHOFFoldCorrect pins that inlining preserves fold semantics for both a
// non-accumulating-shape (sum) and a list-building (cons) kons. Both + and cons
// are capture-safe primitives, so both calls take the inline path; the results
// must match the real left fold.
func TestInlineHOFFoldCorrect(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"sum", `(begin (import (srfi 1)) (fold + 0 '(1 2 3 4 5)))`, "15"},
		{"build", `(begin (import (srfi 1)) (fold cons '() '(1 2 3)))`, "(3 2 1)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := eng.EvalMultiple(ctx, tc.code)
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			got := result.SchemeString()
			if got != tc.want {
				t.Errorf("inlined fold = %s, want %s", got, tc.want)
			}
		})
	}
}

// TestInlineHOFFoldImportedIsStable pins the rebind-stability foundation that
// makes inlining fold's body sound. fold is the one curated HOF that lives in a
// potentially mutable imported library rather than the always-immutable sealed
// base — the "mutability bite" the design flagged (D5): inlining a redefinable
// HOF would let an already-inlined call site diverge from a later redefinition.
// (import (srfi 1)) marks fold Imported, and IsStable() ORs in Imported (R7RS
// forbids set! on imports), so the binding is non-rebindable under BOTH the
// immutable default AND WithMutableTopLevel. tryInlineHOFCall therefore only ever
// inlines a binding the arc's IsStable() contract guarantees will not be rebound,
// the same foundation CallbackIsCaptureSafe relies on for the callback. If a
// future change stopped marking imported fold stable, this fails before an
// unsound inline could ship.
func TestInlineHOFFoldImportedIsStable(t *testing.T) {
	cases := []struct {
		name string
		opt  EngineOption
	}{
		{"immutable-default", WithImmutableTopLevel()},
		{"mutable-top-level", WithMutableTopLevel()},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
				WithLibraryPaths(), tc.opt)
			if err != nil {
				t.Fatal(err)
			}
			_, err = eng.EvalMultiple(ctx, "(import (srfi 1))")
			if err != nil {
				t.Fatalf("import: %v", err)
			}
			b := eng.Environment().GetBinding(values.NewSymbol("fold"), nil)
			if b == nil {
				t.Fatal("fold unbound after (import (srfi 1))")
			}
			if b.InlineHOFParam() != 0 {
				t.Errorf("imported fold InlineHOFParam = %d, want 0 (stamped curated HOF)", b.InlineHOFParam())
			}
			if !b.IsStable() {
				t.Error("imported fold IsStable = false, want true — inlining fold's body is sound only " +
					"because the binding cannot be rebound (Imported); a stamped-but-rebindable HOF would " +
					"let a redefinition diverge from already-inlined call sites")
			}
		})
	}
}

// TestInlineHOFFoldCapturingCallbackCorrect is the soundness boundary for fold: a
// call/cc kons is NOT capture-safe, so the dispatch must refuse to inline and fall
// through to the real fold. The accumulation must remain correct.
func TestInlineHOFFoldCapturingCallbackCorrect(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(begin
(import (srfi 1))
(fold (lambda (x acc) (call/cc (lambda (k) (+ x acc)))) 0 '(1 2 3 4)))`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "10" {
		t.Errorf("fold with a call/cc kons = %s, want 10 "+
			"(must fall through to the real fold, accumulation intact)", got)
	}
}
