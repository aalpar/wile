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
	"testing"

	"github.com/aalpar/wile/pkg/machine"
)

// The depth axis of a LocalIndex — the "up" half of the bit-packed (slot, depth)
// operand — is the last thing standing between the compiler and one-dimensional
// local addressing. plans/flat-closure-baseline.local.md §7d priced its DELETION
// at zero throughput and said to reopen it only for the structural collapse:
// LocalIndex becomes a scalar, Encode/DecodeLocalIndex go away, and the frame
// parent chain stops being a variable-lookup path.
//
// That collapse needs the emitted-depth population to be EMPTY, and it is not.
// This file is the meter for the gap, and the ratchet that keeps it from
// reopening — a depth that comes back is a correct program, so nothing else in
// the suite would notice.
//
// WHAT IS LEFT, AND WHY IT IS NOT AN IMPLEMENTATION TASK. Every remaining
// producer is a closure body that contains an OPAQUE SUBTREE — a quasiquote
// template, or a passthrough form parked in a ValidatedLiteral: cond-expand,
// include, let-syntax, with-syntax, a `syntax` template. Those hold raw,
// UNEXPANDED syntax that this compiler's free-variable pass never looks inside
// (validate.IsOpaqueSubtree), so it cannot name what the body reads, so
// compileClosureBody sets RetainsLexicalEnv and the closure keeps its creating
// frame as its static link and reads outward by depth.
//
// The two ways out are both design problems rather than edits:
//
//   - Enumerate them: validate an opaque subtree in place so the free-variable
//     pass can see it. That moves the expansion/validation split, which is what
//     parks these forms in the first place.
//   - Over-approximate them: put EVERY variable in lexical scope into the free
//     vector. Sound, and a pessimization with no bound on width — and it makes
//     "captured" true for every slot in scope of any body holding a quasiquote,
//     so every one of them that is also assigned becomes boxed.
//
// Until one of those lands, `a flat closure's link is a structural root` reads
// "…unless its body contains code we could not analyse", and the depth axis
// stays.
//
// Measured over the whole pkg/wile suite (instrumenting EncodeLocalIndex, the
// single funnel every local operand reaches) the distinct emit-site count went
// 134 -> 3 -> 2 across the two changes that closed the enumerable populations:
// top-level `let` nesting with its free-var pushes, and the syntax-case clause
// body. Both survivors are the hatch above.

// depthSitesIn counts instructions carrying a non-zero LocalIndex depth across
// tpl and every template in its literal pool.
//
// It reads the DISASSEMBLY rather than the raw Arg because the operand kind is
// what decides whether Arg is a packed (slot, depth) pair at all: OpPushFree and
// OpLoadFree carry a one-dimensional free index in the same field, and decoding
// one as a pair would report a phantom depth.
func depthSitesIn(tpl *machine.NativeTemplate) int {
	q := 0
	seen := map[*machine.NativeTemplate]bool{}
	var walk func(cur *machine.NativeTemplate)
	walk = func(cur *machine.NativeTemplate) {
		if cur == nil || seen[cur] {
			return
		}
		seen[cur] = true
		for _, di := range machine.Disassemble(cur).Instructions {
			if di.Depth > 0 {
				q++
			}
		}
		for _, lit := range cur.Literals() {
			sub, ok := lit.(*machine.NativeTemplate)
			if ok {
				walk(sub)
			}
		}
	}
	walk(tpl)
	return q
}

// TestEmittedLocalDepthIsZeroWhereEnumerable pins that every shape whose outer
// references the free-variable pass CAN enumerate now addresses them at depth 0.
//
// A row that starts emitting depth again has lost a merge or lost a free-vector
// entry, and in both cases still evaluates correctly — which is why this is a
// census rather than a value test. The value arms live in
// TestLetFrameCorpusValues and TestSyntaxCaseClauseBodyLetBehaviour.
func TestEmittedLocalDepthIsZeroWhereEnumerable(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{
			name: "nested lets inside a lambda body",
			code: `((lambda (n) (let ((a n)) (let ((b a)) (let ((c b)) c)))) 1)`,
		},
		{
			name: "nested lets at the top level",
			code: `(let ((a 1)) (let ((b a)) (let ((c b)) (+ a b c))))`,
		},
		{
			name: "a closure over a top-level merged slot",
			code: `(let ((a 1)) (let ((b 2)) ((lambda () (+ a b)))))`,
		},
		{
			// A closure inside a closure: the inner one reaches the outer
			// lambda's parameter through its free vector, never by depth.
			name: "a closure chain three deep",
			code: `((((lambda (x) (lambda (y) (lambda (z) (+ x y z)))) 1) 2) 3)`,
		},
		{
			// Captured AND assigned, so the slot is boxed. OpBoxSlot and
			// OpStoreThroughBox carry a LocalIndex too, and a box installed at
			// the wrong depth is the loudest way to get this wrong.
			name: "a boxed slot read from an inner closure",
			code: `((lambda (a) ((lambda () (set! a 2))) a) 1)`,
		},
		{
			// A named let is a letrec whose init is a lambda; the loop body
			// reaches the accumulator through its own parameter frame.
			name: "a named let loop",
			code: `(let loop ((i 0) (acc 0)) (if (= i 3) acc (loop (+ i 1) (+ acc i))))`,
		},
		{
			// A syntax-case clause body's `let` merges into the pattern-variable
			// frame, so the template that follows it reads at depth 0.
			name: "a let in a syntax-case clause body",
			code: `(lambda (stx) (syntax-case stx () ((_ a) (let ((x 1)) (syntax a)))))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tpl := compileForDepthCensus(t, tc.code)
			got := depthSitesIn(tpl)
			if got != 0 {
				t.Errorf("emitted %d instruction(s) with depth != 0, want 0; "+
					"this shape's outer references are enumerable, so losing a merge "+
					"or a free-vector entry is the only way to get here", got)
			}
		})
	}
}

// TestEmittedLocalDepthSurvivesOnlyOpaqueSubtrees is the other half of the
// ratchet: it pins that the remaining producers are exactly the RetainsLexicalEnv
// hatch, so a future change cannot quietly re-broaden the population and still
// look green.
//
// Each row MUST emit depth, and must do so for the same reason: the body holds
// raw syntax the free-variable pass cannot look inside, so the closure keeps its
// creating frame. If a row stops emitting depth, someone closed the hatch for
// that shape — which is progress, and is exactly the moment to re-price the
// LocalIndex collapse rather than to delete the row.
func TestEmittedLocalDepthSurvivesOnlyOpaqueSubtrees(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{
			// A `syntax` template's pattern-variable reference. It resolves at
			// run time through the frame BindPatternVars pushes, appears in no
			// free layout because nothing walked the template, and the closure
			// that holds it is one frame below.
			name: "a lambda holding a syntax template inside a clause body",
			code: `(lambda (stx)
			         (syntax-case stx ()
			           ((_ a) (let ((f (lambda () (syntax a)))) (f)))))`,
		},
		{
			// A quasiquote template. Same classification, different form: its
			// unquoted expressions are unexpanded syntax at this point.
			name: "a lambda holding a quasiquote inside a let in a clause body",
			code: `(lambda (stx)
			         (syntax-case stx ()
			           ((_ a) (let ((n 1))
			                    (let ((f (lambda () (quasiquote ((unquote n)))))) (f))))))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tpl := compileForDepthCensus(t, tc.code)
			got := depthSitesIn(tpl)
			if got == 0 {
				t.Errorf("emitted no depth != 0 instruction, want at least one; " +
					"if the opaque-subtree hatch was closed for this shape, re-price " +
					"the LocalIndex collapse (baseline §7d) rather than deleting this row")
			}
		})
	}
}

// compileForDepthCensus compiles one form through the public Engine and returns
// its template.
//
// The Engine path, not a hand-built continuation: the merge decisions this
// counts are made by compileBody and CompileValidatedLet wiring that a direct
// call would bypass, and the optimizer runs here (memory/runschemecode-blind-to-
// optimizer).
func compileForDepthCensus(t *testing.T, code string) *machine.NativeTemplate {
	t.Helper()
	ctx := context.Background()
	engine, err := NewEngine(ctx)
	if err != nil {
		t.Fatalf("new engine: %v", err)
	}
	expr, err := engine.Parse(ctx, code)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	compiled, err := engine.Compile(ctx, expr)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	return compiled.template
}
