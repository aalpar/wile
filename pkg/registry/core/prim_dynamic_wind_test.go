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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// dynamic-wind Tests (R7RS §6.4 - Cleanup handlers)

func TestDynamicWindComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic - returns thunk value
		{Name: "returns thunk value", Code: `(dynamic-wind (lambda () #f) (lambda () 42) (lambda () #f))`, Expected: values.NewInteger(42)},

		// Execution order
		{
			Name: "before runs first",
			Code: `(let ((log '()))
				(dynamic-wind
					(lambda () (set! log (cons 'before log)))
					(lambda () (set! log (cons 'during log)) 'result)
					(lambda () (set! log (cons 'after log))))
				(reverse log))`,
			Expected: values.List(values.NewSymbol("before"), values.NewSymbol("during"), values.NewSymbol("after")),
		},

		// After runs even on error (caught)
		{
			Name: "after runs on normal exit",
			Code: `(let ((after-ran #f))
				(dynamic-wind
					(lambda () #f)
					(lambda () 42)
					(lambda () (set! after-ran #t)))
				after-ran)`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestDynamicWindWithContinuations(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// After runs on continuation escape
		{
			Name: "after runs on escape",
			Code: `(let ((after-ran #f))
				(call/cc (lambda (k)
					(dynamic-wind
						(lambda () #f)
						(lambda () (k 'escaped))
						(lambda () (set! after-ran #t)))))
				after-ran)`,
			Expected: values.TrueValue,
		},

		// Escape value is correct
		{
			Name: "escape returns correct value",
			Code: `(call/cc (lambda (k)
				(dynamic-wind
					(lambda () #f)
					(lambda () (k 77))
					(lambda () #f))))`,
			Expected: values.NewInteger(77),
		},

		// Before/after state mutation visible
		{
			Name: "before sets state",
			Code: `(let ((v (make-vector 1 0)))
				(dynamic-wind
					(lambda () (vector-set! v 0 1))
					(lambda () (vector-ref v 0))
					(lambda () (vector-set! v 0 2))))`,
			Expected: values.NewInteger(1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestDynamicWindErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "before not procedure", Code: `(dynamic-wind 5 (lambda () 1) (lambda () 2))`},
		{Name: "thunk not procedure", Code: `(dynamic-wind (lambda () 1) 5 (lambda () 2))`},
		{Name: "after not procedure", Code: `(dynamic-wind (lambda () 1) (lambda () 2) 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestDynamicWindR7RSExample tests the classic R7RS §6.10 example.
// This is the canonical test for dynamic-wind + call/cc interaction.
func TestDynamicWindR7RSExample(t *testing.T) {
	// R7RS §6.10 example:
	// (let ((path '()) (c #f))
	//   (let ((add (lambda (s) (set! path (cons s path)))))
	//     (dynamic-wind
	//       (lambda () (add 'connect))
	//       (lambda () (add (call/cc (lambda (c0) (set! c c0) 'talk1))))
	//       (lambda () (add 'disconnect)))
	//     (if (< (length path) 4)
	//         (c 'talk2)
	//         (reverse path))))
	// => (connect talk1 disconnect connect talk2 disconnect)
	code := `
		(let ((path '()) (c #f))
			(let ((add (lambda (s) (set! path (cons s path)))))
				(dynamic-wind
					(lambda () (add 'connect))
					(lambda () (add (call/cc (lambda (c0) (set! c c0) 'talk1))))
					(lambda () (add 'disconnect)))
				(if (< (length path) 4)
					(c 'talk2)
					(reverse path))))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	expected := values.List(
		values.NewSymbol("connect"),
		values.NewSymbol("talk1"),
		values.NewSymbol("disconnect"),
		values.NewSymbol("connect"),
		values.NewSymbol("talk2"),
		values.NewSymbol("disconnect"),
	)
	qt.Assert(t, result, valuestest.SchemeEquals, expected)
}

// TestDynamicWindNestedContinuations tests nested dynamic-wind with continuations.
func TestDynamicWindNestedContinuations(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Nested dynamic-wind - inner escape triggers both afters
		{
			Name: "nested escape runs both afters",
			Code: `
				(let ((log '()))
					(call/cc (lambda (outer-k)
						(dynamic-wind
							(lambda () (set! log (cons 'outer-before log)))
							(lambda ()
								(dynamic-wind
									(lambda () (set! log (cons 'inner-before log)))
									(lambda () (outer-k 'escaped))
									(lambda () (set! log (cons 'inner-after log)))))
							(lambda () (set! log (cons 'outer-after log))))))
					(reverse log))
			`,
			Expected: values.List(
				values.NewSymbol("outer-before"),
				values.NewSymbol("inner-before"),
				values.NewSymbol("inner-after"),
				values.NewSymbol("outer-after"),
			),
		},
		// Nested dynamic-wind - normal exit
		{
			Name: "nested normal exit",
			Code: `
				(let ((log '()))
					(dynamic-wind
						(lambda () (set! log (cons 'outer-before log)))
						(lambda ()
							(dynamic-wind
								(lambda () (set! log (cons 'inner-before log)))
								(lambda () (set! log (cons 'inner-thunk log)) 'result)
								(lambda () (set! log (cons 'inner-after log)))))
						(lambda () (set! log (cons 'outer-after log))))
					(reverse log))
			`,
			Expected: values.List(
				values.NewSymbol("outer-before"),
				values.NewSymbol("inner-before"),
				values.NewSymbol("inner-thunk"),
				values.NewSymbol("inner-after"),
				values.NewSymbol("outer-after"),
			),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestReentryInsideAFrameKeepsIt pins that applying a continuation inside a
// dynamic-wind keeps the frames the invoker is inside. A composable continuation
// composes its own frames onto the invoker's winding stack; a call/cc continuation
// reinstated at an inner prompt keeps the frames outside that prompt. Both used to
// reconcile to the captured stack as an absolute target, so the invoker's frame was
// unwound (out ran before the body was done) and the body's own PopWind then found
// the stack empty. Racket logs the expected order for both shapes.
func TestReentryInsideAFrameKeepsIt(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "composable continuation",
			Code: `
				(let ((log '()) (kc #f) (dtag (default-continuation-prompt-tag)))
					(define (note x) (set! log (cons x log)))
					(call-with-continuation-prompt
						(lambda () (+ 1 (call-with-composable-continuation (lambda (k) (set! kc k) 0) dtag)))
						dtag (lambda args args))
					(dynamic-wind
						(lambda () (note 'in))
						(lambda () (note (list 'kc-returned (kc 7))) (note 'body-done))
						(lambda () (note 'out)))
					(reverse log))
			`,
			Expected: values.List(values.NewSymbol("in"),
				values.List(values.NewSymbol("kc-returned"), values.NewInteger(8)),
				values.NewSymbol("body-done"), values.NewSymbol("out")),
		},
		{
			Name: "call/cc continuation at an inner prompt",
			Code: `
				(let ((log '()) (k #f) (dtag (default-continuation-prompt-tag)))
					(define (note x) (set! log (cons x log)))
					(call-with-continuation-prompt
						(lambda () (+ 1 (call/cc (lambda (c) (set! k c) 0))))
						dtag (lambda args args))
					(dynamic-wind
						(lambda () (note 'in))
						(lambda ()
							(note (list 'k-returned (call-with-continuation-prompt (lambda () (k 7)) dtag (lambda args args))))
							(note 'body-done))
						(lambda () (note 'out)))
					(reverse log))
			`,
			Expected: values.List(values.NewSymbol("in"),
				values.List(values.NewSymbol("k-returned"), values.NewInteger(8)),
				values.NewSymbol("body-done"), values.NewSymbol("out")),
		},
		{
			// The extents outside the capture's prompt are the capture site's, not
			// the continuation's: applying it elsewhere neither re-enters them (in
			// again) nor leaves them on the stack for the run's end to exit (out again).
			Name: "composable continuation carries only the extents inside its prompt",
			Code: `
				(let ((log '()) (kc #f) (dtag (default-continuation-prompt-tag)))
					(define (note x) (set! log (cons x log)))
					(dynamic-wind
						(lambda () (note 'in))
						(lambda () (call-with-continuation-prompt
							(lambda () (call-with-composable-continuation (lambda (k) (set! kc k) 0) dtag))
							dtag (lambda args args)))
						(lambda () (note 'out)))
					(kc 7)
					(reverse log))
			`,
			Expected: values.List(values.NewSymbol("in"), values.NewSymbol("out")),
		},
		{
			Name: "call/cc continuation carries only the extents inside its prompt",
			Code: `
				(let ((log '()) (k #f) (dtag (default-continuation-prompt-tag)))
					(define (note x) (set! log (cons x log)))
					(dynamic-wind
						(lambda () (note 'in))
						(lambda () (call-with-continuation-prompt
							(lambda () (call/cc (lambda (c) (set! k c) 0)))
							dtag (lambda args args)))
						(lambda () (note 'out)))
					(call-with-continuation-prompt (lambda () (k 7)) dtag (lambda args args))
					(reverse log))
			`,
			Expected: values.List(values.NewSymbol("in"), values.NewSymbol("out")),
		},
		{
			// A prompt frame inside the segment recorded the capture site's winding
			// stack; reinstated under another invoker, the record is rebased onto the
			// invoker's. Unrebased, the abort to that prompt reconciled to the capture
			// site's stack, unwinding the invoker's frame ("pop-wind: winding stack is
			// empty" when the body then left it).
			Name: "prompt carried inside a composable continuation, aborted to",
			Code: `
				(let ((log '()) (kc #f) (dtag (default-continuation-prompt-tag))
				      (t (make-continuation-prompt-tag 't)))
					(define (note x) (set! log (cons x log)))
					(call-with-continuation-prompt
						(lambda ()
							(call-with-continuation-prompt
								(lambda ()
									(let ((v (call-with-composable-continuation (lambda (k) (set! kc k) 0) t)))
										(if (= v 0) 0 (abort-current-continuation dtag 'aborted))))
								dtag (lambda args (cons 'q-handler args))))
						t (lambda args args))
					(dynamic-wind
						(lambda () (note 'in))
						(lambda () (note (list 'kc-returned (kc 5))) (note 'body-done))
						(lambda () (note 'out)))
					(reverse log))
			`,
			Expected: values.List(values.NewSymbol("in"),
				values.List(values.NewSymbol("kc-returned"),
					values.List(values.NewSymbol("q-handler"), values.NewSymbol("aborted"))),
				values.NewSymbol("body-done"), values.NewSymbol("out")),
		},
		{
			// The same rebasing for a call/cc resume at the carried prompt: Racket logs
			// this order; unrebased, the invoker's after thunk ran mid-body and twice.
			Name: "prompt carried inside a composable continuation, resumed at",
			Code: `
				(let ((log '()) (kc #f) (dtag (default-continuation-prompt-tag))
				      (t (make-continuation-prompt-tag 't)))
					(define (note x) (set! log (cons x log)))
					(call-with-continuation-prompt
						(lambda ()
							(dynamic-wind
								(lambda () (note 'in-cap))
								(lambda ()
									(call-with-continuation-prompt
										(lambda ()
											(let ((v (call-with-composable-continuation (lambda (k) (set! kc k) 0) t)))
												(if (= v 0) 0 (+ 100 (call/cc (lambda (c) (c 1)))))))
										dtag (lambda args (cons 'q-handler args))))
								(lambda () (note 'out-cap))))
						t (lambda args args))
					(note '--)
					(dynamic-wind
						(lambda () (note 'in-inv))
						(lambda () (note (list 'kc-returned (kc 5))) (note 'body-done))
						(lambda () (note 'out-inv)))
					(reverse log))
			`,
			Expected: values.List(values.NewSymbol("in-cap"), values.NewSymbol("out-cap"), values.NewSymbol("--"),
				values.NewSymbol("in-inv"), values.NewSymbol("in-cap"), values.NewSymbol("out-cap"),
				values.List(values.NewSymbol("kc-returned"), values.NewInteger(101)),
				values.NewSymbol("body-done"), values.NewSymbol("out-inv")),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestRootlessSubContextLeavesInheritedExtents pins that a tail call/cc in a
// winding thunk leaves the extents the thunk's sub-context inherited to the
// context that entered them. The call/cc once ran its procedure on a rootless
// sub-context under a RunResumable driver of its own, whose completion unwound the
// whole inherited stack, so every enclosing extent's after thunk ran there and
// again when the escape left it; it is now applied in place on the thunk's
// sub-context. A continuation captured and applied inside it reinstates at that
// sub-context's own boundary, which keeps the inherited extents too.
func TestRootlessSubContextLeavesInheritedExtents(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "completion",
			Code: `
				(let ((log '()))
					(define (note x) (set! log (cons x log)))
					(dynamic-wind
						(lambda () (note 'in))
						(lambda () (call/cc (lambda (k0)
							(dynamic-wind (lambda () #f)
								(lambda () (k0 'esc))
								(lambda () (call/cc (lambda (k) (note 'inner) 'x)))))))
						(lambda () (note 'out)))
					(reverse log))
			`,
			Expected: values.List(values.NewSymbol("in"), values.NewSymbol("inner"), values.NewSymbol("out")),
		},
		{
			// A continuation captured outside the after thunk's sub-context and resumed
			// inside it replaces the escape in flight: the extents are kept once, not
			// re-entered on top of themselves, and the rest of the program runs once.
			// The whole log is asserted: a second run of the remainder (the defect
			// this replaced) appends to it.
			Name: "continuation captured outside, resumed inside",
			Code: `
				(let ((log '()) (k #f) (n 0))
					(define (note x) (set! log (cons x log)))
					(dynamic-wind (lambda () (note 'in))
						(lambda ()
							(let ((v (call/cc (lambda (c) (set! k c) 'first))))
								(note v)
								(if (< n 1)
									(begin (set! n (+ n 1))
										(call/cc (lambda (k0)
											(dynamic-wind (lambda () #f)
												(lambda () (k0 'esc))
												(lambda () (call/cc (lambda (c2) (k 'again))))))))
									'done)))
						(lambda () (note 'out)))
					(reverse log))
			`,
			Expected: values.List(values.NewSymbol("in"), values.NewSymbol("first"),
				values.NewSymbol("again"), values.NewSymbol("out")),
		},
		{
			Name: "continuation applied at its own boundary",
			Code: `
				(let ((log '()))
					(define (note x) (set! log (cons x log)))
					(dynamic-wind
						(lambda () (note 'in))
						(lambda () (call/cc (lambda (k0)
							(dynamic-wind (lambda () #f)
								(lambda () (k0 'esc))
								(lambda () (call/cc (lambda (k) (k 'x))))))))
						(lambda () (note 'out)))
					(reverse log))
			`,
			Expected: values.List(values.NewSymbol("in"), values.NewSymbol("out")),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestDynamicWindContinuationReentry tests calling a captured continuation multiple times.
func TestDynamicWindContinuationReentry(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Re-entry: continuation invoked twice
		{
			Name: "continuation invoked twice",
			Code: `
				(let ((count 0) (c #f))
					(dynamic-wind
						(lambda () (set! count (+ count 1)))
						(lambda () (call/cc (lambda (k) (set! c k) 'first)))
						(lambda () #f))
					(if (< count 3)
						(c 'again)
						count))
			`,
			Expected: values.NewInteger(3),
		},
		// Re-entry tracks before calls
		{
			Name: "reentry runs before thunk each time",
			Code: `
				(let ((before-count 0) (c #f) (iterations 0))
					(dynamic-wind
						(lambda () (set! before-count (+ before-count 1)))
						(lambda () (call/cc (lambda (k) (set! c k) 'first)))
						(lambda () #f))
					(set! iterations (+ iterations 1))
					(if (< iterations 3)
						(c 'again)
						before-count))
			`,
			Expected: values.NewInteger(3),
		},
		// Re-entry tracks after calls
		{
			Name: "reentry runs after thunk on exit",
			Code: `
				(let ((after-count 0) (c #f) (iterations 0))
					(dynamic-wind
						(lambda () #f)
						(lambda () (call/cc (lambda (k) (set! c k) 'first)))
						(lambda () (set! after-count (+ after-count 1))))
					(set! iterations (+ iterations 1))
					(if (< iterations 3)
						(c 'again)
						after-count))
			`,
			Expected: values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestCallCCComprehensive tests various call/cc scenarios.
func TestCallCCComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic: return without invoking continuation
		{
			Name:     "normal return",
			Code:     `(call/cc (lambda (k) 42))`,
			Expected: values.NewInteger(42),
		},
		// Basic: invoke continuation immediately
		{
			Name:     "immediate escape",
			Code:     `(call/cc (lambda (k) (k 42)))`,
			Expected: values.NewInteger(42),
		},
		// Continuation skips remaining computation
		{
			Name:     "escape skips computation",
			Code:     `(+ 1 (call/cc (lambda (k) (+ 100 (k 10)))))`,
			Expected: values.NewInteger(11), // 1 + 10, not 1 + 100 + 10
		},
		// Continuation captured and stored for later
		{
			Name: "stored continuation invoked later",
			Code: `
				(let ((saved #f))
					(+ 1 (call/cc (lambda (k) (set! saved k) 10)))
					(if saved
						(let ((k saved))
							(set! saved #f)
							(k 20))
						'done))
			`,
			Expected: values.NewSymbol("done"),
		},
		// Nested call/cc
		{
			Name: "nested call/cc inner escape",
			Code: `
				(call/cc (lambda (outer)
					(call/cc (lambda (inner)
						(inner 'from-inner)))
					'after-inner))
			`,
			Expected: values.NewSymbol("after-inner"),
		},
		{
			Name: "nested call/cc outer escape",
			Code: `
				(call/cc (lambda (outer)
					(call/cc (lambda (inner)
						(outer 'from-outer)))
					'after-inner))
			`,
			Expected: values.NewSymbol("from-outer"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestDynamicWindMultipleValues pins R7RS §6.4: dynamic-wind returns whatever
// its thunk returns, INCLUDING zero or multiple values. The compiled
// dynamic-wind bytecode (CompileValidatedDynamicWind) saved the thunk result as
// a single value via PUSH/PEEK, so a thunk returning (values …) corrupted the
// stack — N>1 raised "expected a procedure" (after-thunk APPLY read a misaligned
// slot) and N==0 underflowed the stack.
func TestDynamicWindMultipleValues(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "thunk returns two values",
			Code:     `(call-with-values (lambda () (dynamic-wind (lambda () #f) (lambda () (values 3 4)) (lambda () #f))) list)`,
			Expected: values.List(values.NewInteger(3), values.NewInteger(4)),
		},
		{
			Name:     "thunk returns three values into a consumer",
			Code:     `(call-with-values (lambda () (dynamic-wind (lambda () #f) (lambda () (values 1 2 3)) (lambda () #f))) +)`,
			Expected: values.NewInteger(6),
		},
		{
			Name:     "thunk returns zero values",
			Code:     `(call-with-values (lambda () (dynamic-wind (lambda () #f) (lambda () (values)) (lambda () #f))) (lambda () 'zero))`,
			Expected: values.NewSymbol("zero"),
		},
		{
			Name:     "single value still works",
			Code:     `(dynamic-wind (lambda () #f) (lambda () 7) (lambda () #f))`,
			Expected: values.NewInteger(7),
		},
		{
			// The before/after thunks must still run when the body yields
			// multiple values (winding is not skipped by the multi-value path).
			Name: "before and after run with multiple-value body",
			Code: `(let ((log '()))
			          (call-with-values
			            (lambda () (dynamic-wind
			                         (lambda () (set! log (cons 'before log)))
			                         (lambda () (values 1 2))
			                         (lambda () (set! log (cons 'after log)))))
			            (lambda (a b) (set! log (cons (+ a b) log))))
			          (reverse log))`,
			Expected: values.List(values.NewSymbol("before"), values.NewSymbol("after"), values.NewInteger(3)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
