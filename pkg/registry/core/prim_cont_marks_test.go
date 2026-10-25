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
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

func TestPrimContinuationMarkSetQ(t *testing.T) {
	t.Parallel()

	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "mark set", Code: `(continuation-mark-set? (current-continuation-marks))`, Expected: values.TrueValue},
		{Name: "integer", Code: `(continuation-mark-set? 42)`, Expected: values.FalseValue},
		{Name: "string", Code: `(continuation-mark-set? "hello")`, Expected: values.FalseValue},
		{Name: "list", Code: `(continuation-mark-set? '(1 2))`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestPrimContinuationMarkSetToList(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name:     "no marks",
			code:     `(continuation-mark-set->list (current-continuation-marks) 'k)`,
			expected: "()",
		},
		{
			name: "single mark",
			code: `(with-continuation-mark 'k 1
				(continuation-mark-set->list (current-continuation-marks) 'k))`,
			expected: "(1)",
		},
		{
			name: "tail replacement same key",
			code: `(with-continuation-mark 'k 1
				(with-continuation-mark 'k 2
					(continuation-mark-set->list (current-continuation-marks) 'k)))`,
			expected: "(2)",
		},
		{
			name: "different keys",
			code: `(with-continuation-mark 'a 1
				(with-continuation-mark 'b 2
					(continuation-mark-set->list (current-continuation-marks) 'a)))`,
			expected: "(1)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

func TestPrimContinuationMarkSetFirst(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name:     "no marks returns false",
			code:     `(continuation-mark-set-first (current-continuation-marks) 'k)`,
			expected: "#f",
		},
		{
			name:     "custom default",
			code:     `(continuation-mark-set-first (current-continuation-marks) 'k 'custom)`,
			expected: "custom",
		},
		{
			name: "returns value",
			code: `(with-continuation-mark 'k 42
				(continuation-mark-set-first (current-continuation-marks) 'k))`,
			expected: "42",
		},
		{
			name: "returns nearest",
			code: `(with-continuation-mark 'k 1
				(with-continuation-mark 'k 2
					(continuation-mark-set-first (current-continuation-marks) 'k)))`,
			expected: "2",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

func TestPrimContMarkErrors(t *testing.T) {
	t.Parallel()

	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "->list wrong type", Code: `(continuation-mark-set->list 42 'k)`},
		{Name: "first wrong type", Code: `(continuation-mark-set-first 42 'k)`},
		{Name: "current-continuation-marks wrong tag", Code: `(current-continuation-marks 42)`},
		{Name: "continuation-marks wrong type", Code: `(continuation-marks 42)`},
		{Name: "continuation-marks wrong tag", Code: `(call/cc (lambda (k) (continuation-marks k 42)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestPrimContinuationMarks(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name: "no marks on captured continuation",
			code: `(continuation-mark-set->list
				(call/cc (lambda (k) (continuation-marks k)))
				'x)`,
			expected: "()",
		},
		{
			name: "marks visible on captured continuation",
			code: `(with-continuation-mark 'k 42
				(call/cc (lambda (cont)
					(continuation-mark-set->list
						(continuation-marks cont)
						'k))))`,
			expected: "(42)",
		},
		{
			name: "continuation-mark-set? on result",
			code: `(continuation-mark-set?
				(call/cc (lambda (k) (continuation-marks k))))`,
			expected: "#t",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

// TestContinuationMarks_TailBoundaryRecordsMarkOnce pins that a mark set by a
// tail-position with-continuation-mark belongs to ONE frame when the body installs
// a reified boundary (prompt, call-with-values, barrier, raise-continuable handler).
// The boundary frame used to copy the caller's live marks while the body kept them
// too, so every such mark was recorded twice; and a tag-bounded collection included
// the prompt frame's marks, which belong outside the prompt. Expected values are
// Racket's. Each case wraps the wcm in a lambda body, where it is in tail position.
//
// call-with-immediate-continuation-mark is not pinned here: inside a prompt body it
// answers the caller's mark where Racket answers the default, the same documented
// divergence (I167, docs/reference/r7rs-differences.md) that
// TestContinuationMarks_ShapeMatrix pins for the non-tail call. Both come from
// GetImmediateMark's fallback to p.cont.
func TestContinuationMarks_TailBoundaryRecordsMarkOnce(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name: "list under prompt",
			code: `((lambda () (with-continuation-mark 'a 'outer
				(call-with-continuation-prompt
					(lambda () (continuation-mark-set->list (current-continuation-marks) 'a))
					(make-continuation-prompt-tag) #f))))`,
			expected: "(outer)",
		},
		{
			name: "list* under prompt with inner key",
			code: `((lambda () (with-continuation-mark 'a 'outer
				(call-with-continuation-prompt
					(lambda () (with-continuation-mark 'b 'x
						(continuation-mark-set->list* (current-continuation-marks) '(a b))))
					(make-continuation-prompt-tag) #f))))`,
			expected: "(#(#f x) #(outer #f))",
		},
		{
			name: "nested prompts",
			code: `((lambda () (with-continuation-mark 'a 1
				(call-with-continuation-prompt
					(lambda () (with-continuation-mark 'a 2
						(call-with-continuation-prompt
							(lambda () (continuation-mark-set->list (current-continuation-marks) 'a))
							(make-continuation-prompt-tag) #f)))
					(make-continuation-prompt-tag) #f))))`,
			expected: "(2 1)",
		},
		{
			name: "list bounded by the prompt's own tag",
			code: `(let ((tag (make-continuation-prompt-tag)))
				((lambda () (with-continuation-mark 'a 'outer
					(call-with-continuation-prompt
						(lambda () (continuation-mark-set->list (current-continuation-marks tag) 'a))
						tag #f)))))`,
			expected: "()",
		},
		{
			name: "first bounded by the prompt's own tag",
			code: `(let ((tag (make-continuation-prompt-tag)))
				((lambda () (with-continuation-mark 'a 'outer
					(call-with-continuation-prompt
						(lambda () (continuation-mark-set-first (current-continuation-marks tag) 'a 'none))
						tag #f)))))`,
			expected: "none",
		},
		{
			name: "list bounded by the default tag under a default prompt",
			code: `((lambda () (with-continuation-mark 'a 'outer
				(call-with-continuation-prompt
					(lambda () (continuation-mark-set->list (current-continuation-marks) 'a))
					(default-continuation-prompt-tag) #f))))`,
			expected: "()",
		},
		{
			name: "call-with-values producer",
			code: `((lambda () (with-continuation-mark 'a 'outer
				(call-with-values
					(lambda () (continuation-mark-set->list (current-continuation-marks) 'a))
					(lambda (x) x)))))`,
			expected: "(outer)",
		},
		{
			name: "barrier",
			code: `((lambda () (with-continuation-mark 'a 'outer
				(call-with-continuation-barrier
					(lambda () (continuation-mark-set->list (current-continuation-marks) 'a))))))`,
			expected: "(outer)",
		},
		{
			name: "raise-continuable handler",
			code: `(with-exception-handler
				(lambda (c) (continuation-mark-set->list (current-continuation-marks) 'a))
				(lambda () (with-continuation-mark 'a 'outer (raise-continuable 'x))))`,
			expected: "(outer)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

func TestPrimContinuationQ(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name:     "captured continuation is true",
			code:     `(call/cc (lambda (k) (continuation? k)))`,
			expected: "#t",
		},
		{
			name:     "lambda is false",
			code:     `(continuation? (lambda () 1))`,
			expected: "#f",
		},
		{
			name:     "integer is false",
			code:     `(continuation? 42)`,
			expected: "#f",
		},
		{
			name:     "procedure? on continuation is true",
			code:     `(call/cc (lambda (k) (procedure? k)))`,
			expected: "#t",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

func TestPrimCallWithImmediateContMark(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name:     "no mark uses default #f",
			code:     `(call-with-immediate-continuation-mark 'k (lambda (v) v))`,
			expected: "#f",
		},
		{
			name:     "no mark uses explicit default",
			code:     `(call-with-immediate-continuation-mark 'k (lambda (v) v) 'missing)`,
			expected: "missing",
		},
		{
			name: "reads mark from current frame",
			code: `(with-continuation-mark 'k 99
				(call-with-immediate-continuation-mark 'k (lambda (v) v)))`,
			expected: "99",
		},
		{
			name: "tail replacement — sees only innermost",
			code: `(with-continuation-mark 'k 1
				(with-continuation-mark 'k 2
					(call-with-immediate-continuation-mark 'k (lambda (v) v))))`,
			expected: "2",
		},
		{
			name: "different key returns default",
			code: `(with-continuation-mark 'k 1
				(call-with-immediate-continuation-mark 'other (lambda (v) v)))`,
			expected: "#f",
		},
		{
			name: "proc result is returned",
			code: `(call-with-immediate-continuation-mark 'k
				(lambda (v) (+ 1 2)))`,
			expected: "3",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

// markShapeRunners evaluates code down both pipelines. testhelpers.RunSchemeCode
// never calls NativeTemplate.Optimize, so a table written only through it exercises
// the unoptimized bytecode while the CLI and every embedder run the optimized one;
// the Engine path is the one a user sees. A shape that answers differently on the
// two is itself a finding, so each row runs on both. Each runner takes the subtest's
// t, so a setup failure fails that subtest and not the parent.
func markShapeRunners() map[string]func(t *testing.T, code string) (string, error) {
	return map[string]func(t *testing.T, code string) (string, error){
		"unoptimized": func(t *testing.T, code string) (string, error) {
			t.Helper()
			result, err := testhelpers.RunSchemeCode(t, code)
			if err != nil {
				return "", err
			}
			return result.SchemeString(), nil
		},
		"engine": func(t *testing.T, code string) (string, error) {
			t.Helper()
			engine, err := wile.NewEngine(context.Background())
			qt.Assert(t, err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), code)
			if err != nil {
				return "", err
			}
			return result.SchemeString(), nil
		},
	}
}

// TestContinuationMarkSetAcceptsFalse pins Racket's alias: #f stands for the
// current continuation's marks in the first argument of every continuation-mark-set
// accessor. Expected values are Racket's, measured: 42, (7), (#(7)); the two list
// forms return a list and a list of vectors. The two-frame rows put a mark in an
// enclosing frame and one in the live frame, so a #f path that walked only the
// chain, or only the live frame, answers differently.
func TestContinuationMarkSetAcceptsFalse(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name:     "first",
			code:     `(with-continuation-mark 'k 42 (continuation-mark-set-first #f 'k))`,
			expected: "42",
		},
		{
			name:     "first with default and no mark",
			code:     `(continuation-mark-set-first #f 'k 'none)`,
			expected: "none",
		},
		{
			name:     "list",
			code:     `(with-continuation-mark 'k 7 (continuation-mark-set->list #f 'k))`,
			expected: "(7)",
		},
		{
			name:     "list star",
			code:     `(with-continuation-mark 'k 7 (continuation-mark-set->list* #f '(k)))`,
			expected: "(#(7))",
		},
		{
			name:     "same answer as the explicit current set",
			code:     `(with-continuation-mark 'k 7 (equal? (continuation-mark-set->list #f 'k) (continuation-mark-set->list (current-continuation-marks) 'k)))`,
			expected: "#t",
		},
		{
			name: "list, two frames",
			code: `((lambda () (with-continuation-mark 'k 1 (list ((lambda ()
				(with-continuation-mark 'k 2 (continuation-mark-set->list #f 'k))))))))`,
			expected: "((2 1))",
		},
		{
			name: "list star, two frames",
			code: `((lambda () (with-continuation-mark 'k 1 (list ((lambda ()
				(with-continuation-mark 'k 2 (continuation-mark-set->list* #f '(k)))))))))`,
			expected: "((#(2) #(1)))",
		},
		{
			// Kills a chain-only walk (it answers (1)); a live-only walk also answers
			// (2), so the list rows are what pin the chain half.
			name: "first, two frames",
			code: `((lambda () (with-continuation-mark 'k 1 (list ((lambda ()
				(with-continuation-mark 'k 2 (continuation-mark-set-first #f 'k))))))))`,
			expected: "(2)",
		},
	}
	for name, run := range markShapeRunners() {
		for _, tc := range tcs {
			t.Run(name+"/"+tc.name, func(t *testing.T) {
				got, err := run(t, tc.code)
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, got, qt.Equals, tc.expected)
			})
		}
	}
}

// TestContinuationMarkSetFalseStillRejectsOtherTypes pins that #f is the only
// non-set value the accessors take.
func TestContinuationMarkSetFalseStillRejectsOtherTypes(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name string
		code string
	}{
		{name: "first 0", code: `(continuation-mark-set-first 0 'k)`},
		{name: "first true", code: `(continuation-mark-set-first #t 'k)`},
		{name: "list empty list", code: `(continuation-mark-set->list '() 'k)`},
		{name: "list true", code: `(continuation-mark-set->list #t 'k)`},
		{name: "list star symbol", code: `(continuation-mark-set->list* 'k '(k))`},
		{name: "list star zero", code: `(continuation-mark-set->list* 0 '(k))`},
	}
	for name, run := range markShapeRunners() {
		for _, tc := range tcs {
			t.Run(name+"/"+tc.name, func(t *testing.T) {
				_, err := run(t, tc.code)
				qt.Assert(t, err, qt.ErrorIs, werr.ErrNotAContinuationMarkSet)
			})
		}
	}
}

// TestContinuationMarks_OrderAndAliasing pins, end to end, that (continuation-marks k)
// lists the live frame's mark before the chain's (order) and that it snapshots the
// live marks at capture (a wcm after the capture does not show in k's set).
// Expected values are Racket's.
func TestContinuationMarks_OrderAndAliasing(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name: "live frame first",
			code: `((lambda () (with-continuation-mark 'a 1 (list ((lambda ()
				(with-continuation-mark 'a 2 (call/cc (lambda (k)
					(continuation-mark-set->list (continuation-marks k) 'a))))))))))`,
			expected: "((2 1))",
		},
		{
			name: "capture is a snapshot",
			code: `((lambda () (with-continuation-mark 'a 1 (call/cc (lambda (k)
				(with-continuation-mark 'a 2 (list
					(continuation-mark-set->list (continuation-marks k) 'a)
					(continuation-mark-set->list (current-continuation-marks) 'a))))))))`,
			expected: "((1) (2))",
		},
	}
	for name, run := range markShapeRunners() {
		for _, tc := range tcs {
			t.Run(name+"/"+tc.name, func(t *testing.T) {
				got, err := run(t, tc.code)
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, got, qt.Equals, tc.expected)
			})
		}
	}
}

// TestContinuationMarks_ShapeMatrix is the five-shape matrix: a mark recorded by
// with-continuation-mark, observed four ways from a lambda body (so the wcm is in
// tail position of its activation) and once from a non-tail frame. Four rows are
// Racket's answer. The fifth, cicm-nontail, is a documented divergence (I167): Wile
// reports the enclosing frame's mark where Racket reports the default; its fix tags
// the save-continuation emitted for a call in tail position of a wcm body and adds a
// field to the pooled MachineContinuation, which is the VM's hottest instruction, for
// a Racket extension no consumer has asked for. The row pins Wile's answer so a change
// is deliberate. The k-nontail row catches the "prepend the
// key-collapsed capturedMarks" repair; the "just delete GetImmediateMark's p.cont
// fallback" repair is caught by the cicm-nontail pin flipping and by
// TestPrimCallWithImmediateContMark "reads mark from current frame".
func TestContinuationMarks_ShapeMatrix(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name: "cicm-tail",
			code: `((lambda () (with-continuation-mark 'a 42
				(call-with-immediate-continuation-mark 'a (lambda (v) v) 'D))))`,
			expected: "42",
		},
		{
			name: "ccm-tail",
			code: `((lambda () (with-continuation-mark 'a 2
				(continuation-mark-set->list (current-continuation-marks) 'a))))`,
			expected: "(2)",
		},
		{
			name: "k-nontail",
			code: `((lambda () (with-continuation-mark 'a 2
				(list (call-with-current-continuation
					(lambda (k) (continuation-mark-set->list (continuation-marks k) 'a)))))))`,
			expected: "((2))",
		},
		{
			name: "k-tail",
			code: `((lambda () (with-continuation-mark 'a 2
				(call-with-current-continuation
					(lambda (k) (continuation-mark-set->list (continuation-marks k) 'a))))))`,
			expected: "(2)",
		},
		{
			name: "cicm-nontail is a documented divergence, Racket answers (D)",
			code: `((lambda () (with-continuation-mark 'a 1
				(list (call-with-immediate-continuation-mark 'a (lambda (v) v) 'D)))))`,
			expected: "(1)",
		},
	}
	for name, run := range markShapeRunners() {
		for _, tc := range tcs {
			t.Run(name+"/"+tc.name, func(t *testing.T) {
				got, err := run(t, tc.code)
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, got, qt.Equals, tc.expected)
			})
		}
	}
}

// TestContinuationMarks_NonTailWCMPushesFrame pins A23: the body of a with-continuation-mark
// out of tail position runs in a continuation frame of its own, so a mark it sets sits
// beside the enclosing frame's marks instead of overwriting them. Expected values are
// Racket's, measured. In every RED row the wcm is an argument of a tail call (or a
// non-final body form), so its key/value are evaluated in the frame whose marks it would
// overwrite; the rows marked "control" answered the same before the fix and guard the
// frame's return path (value register, env, tail calls out of the body).
func TestContinuationMarks_NonTailWCMPushesFrame(t *testing.T) {
	t.Parallel()

	const g = `(g (lambda () (continuation-mark-set->list (current-continuation-marks) 'a)))`
	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name:     "same key, two frames",
			code:     `((lambda () (with-continuation-mark 'a 1 (list (with-continuation-mark 'a 2 (continuation-mark-set->list #f 'a))))))`,
			expected: "((2 1))",
		},
		{
			name:     "two keys, two frames",
			code:     `((lambda () (with-continuation-mark 'a 1 (list (with-continuation-mark 'b 2 (continuation-mark-set->list* #f '(a b)))))))`,
			expected: "((#(#f 2) #(1 #f)))",
		},
		{
			name:     "body tail-calls a closure",
			code:     `(let (` + g + `) ((lambda () (with-continuation-mark 'a 1 (list (with-continuation-mark 'a 2 (g)))))))`,
			expected: "((2 1))",
		},
		{
			name:     "body tail-calls through a let-bound alias",
			code:     `(let (` + g + `) ((lambda () (with-continuation-mark 'a 1 (list (with-continuation-mark 'a 2 (let ((h g)) (h))))))))`,
			expected: "((2 1))",
		},
		{
			name: "a tail wcm in the callee overwrites the body's frame, not the outer one",
			code: `(let ((f (lambda () (with-continuation-mark 'a 3 (continuation-mark-set->list #f 'a)))))
				((lambda () (with-continuation-mark 'a 1 (list (with-continuation-mark 'a 2 (f)))))))`,
			expected: "((3 1))",
		},
		{
			name:     "a tail wcm under a let in the body shares the body's frame",
			code:     `((lambda () (with-continuation-mark 'a 1 (list (with-continuation-mark 'a 2 (let ((x 5)) (with-continuation-mark 'a 3 (continuation-mark-set->list #f 'a))))))))`,
			expected: "((3 1))",
		},
		{
			name:     "a non-final wcm in the body restores the body's mark",
			code:     `((lambda () (with-continuation-mark 'a 1 (list (with-continuation-mark 'a 2 (begin (with-continuation-mark 'a 3 'x) (continuation-mark-set->list #f 'a)))))))`,
			expected: "((2 1))",
		},
		{
			name: "continuation-marks of a k captured in the body",
			code: `((lambda () (with-continuation-mark 'a 1 (list (with-continuation-mark 'a 2
				(call-with-current-continuation (lambda (k) (continuation-mark-set->list (continuation-marks k) 'a))))))))`,
			expected: "((2 1))",
		},
		{
			name:     "compound argument of a promoted primitive",
			code:     `(let (` + g + `) ((lambda () (with-continuation-mark 'a 1 (+ 1 (with-continuation-mark 'a 2 (length (g))))))))`,
			expected: "3",
		},
		{
			name:     "call-with-immediate-continuation-mark in value position sees the new frame",
			code:     `((lambda () (with-continuation-mark 'a 1 (list (with-continuation-mark 'b (call-with-immediate-continuation-mark 'a (lambda (v) v) 'D) (continuation-mark-set->list* #f '(a b)))))))`,
			expected: "((#(#f D) #(1 #f)))",
		},
		{
			name:     "control: the outer frame's mark is intact after the body returns",
			code:     `((lambda () (with-continuation-mark 'a 1 (let ((r (list (with-continuation-mark 'a 2 'x)))) (continuation-mark-set->list #f 'a)))))`,
			expected: "(1)",
		},
		{
			name:     "control: tail-called primitives return through the body's frame",
			code:     `((lambda (x) (list (with-continuation-mark 'a 2 (car x)) (with-continuation-mark 'a 3 (+ (car x) 1)))) '(5))`,
			expected: "(5 6)",
		},
		{
			name: "control: a closure escaping a let in the body outlives the frame",
			code: `(let ((mk (lambda (n) (list (with-continuation-mark 'a n (let ((x n)) (lambda () x)))))))
				(let loop ((i 0) (acc 0)) (if (= i 2000) acc (loop (+ i 1) (+ acc ((car (mk i))))))))`,
			expected: "1999000",
		},
		{
			name: "control: a self-call in the body",
			code: `(letrec ((count (lambda (n acc) (if (= n 0) acc
				(count (- n 1) (cons (with-continuation-mark 'a n (count 0 (continuation-mark-set->list #f 'a))) acc))))))
				(count 3 '()))`,
			expected: "((1) (2) (3))",
		},
		{
			name:     "control: one frame per recursion level",
			code:     `(let loop ((i 0)) (if (= i 3) (continuation-mark-set->list #f 'a) (list (with-continuation-mark 'a i (loop (+ i 1))))))`,
			expected: "((((2 1 0))))",
		},
		{
			name:     "control: nested parameterize",
			code:     `(let ((p (make-parameter 1))) (list (parameterize ((p 2)) (list (parameterize ((p 3)) (p)) (p))) (p)))`,
			expected: "((3 2) 1)",
		},
	}
	for name, run := range markShapeRunners() {
		for _, tc := range tcs {
			t.Run(name+"/"+tc.name, func(t *testing.T) {
				got, err := run(t, tc.code)
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, got, qt.Equals, tc.expected)
			})
		}
	}
}

// TestContinuationMarkSetToContextStaysStrict pins that (wile control)'s
// continuation-mark-set->context, which delegates to ->list, does not inherit the
// #f alias: Racket raises a contract violation there, and Wile raises the located
// ErrNotAContinuationMarkSet, as before. Engine-only: the unoptimized runner has no
// library resolver, so (import (wile control)) cannot load there.
func TestContinuationMarkSetToContextStaysStrict(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name     string
		code     string
		expected string
		wantErr  bool
	}{
		{
			name:     "mark set",
			code:     `(with-continuation-mark 'wile/source-location "x" (continuation-mark-set->context (current-continuation-marks)))`,
			expected: `("x")`,
		},
		{name: "false is rejected", code: `(continuation-mark-set->context #f)`, wantErr: true},
		{name: "integer is rejected", code: `(continuation-mark-set->context 0)`, wantErr: true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := wile.NewEngine(context.Background(),
				wile.WithProfile(wile.KitchenSink), wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths())
			qt.Assert(t, err, qt.IsNil)
			_, err = engine.EvalMultiple(context.Background(), `(import (wile control))`)
			qt.Assert(t, err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.code)
			if tc.wantErr {
				qt.Assert(t, err, qt.ErrorIs, werr.ErrNotAContinuationMarkSet)
				return
			}
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

// TestContinuationMarkSetIteratorRacketProtocol pins (wile control)'s iterator
// to Racket's protocol: each call answers two values, the next frame's vector
// (#f once exhausted) and the iterator for the rest, and an iterator is pure.
// Wile's iterator used to be a stateful closure answering one value, a
// different protocol under Racket's name. Expected values measured on racket.
func TestContinuationMarkSetIteratorRacketProtocol(t *testing.T) {
	t.Parallel()

	tcs := []struct{ name, code, expected string }{
		{"two values per call",
			`(with-continuation-mark 'a 1 (let ((it (continuation-mark-set->iterator (current-continuation-marks) '(a)))) (call-with-values it (lambda (v next) (list v (procedure? next))))))`,
			"(#(1) #t)"},
		{"walk frames through the next iterator",
			`(define (g) (with-continuation-mark 'a 2 (let loop ((it (continuation-mark-set->iterator (current-continuation-marks) '(a)))) (call-with-values it (lambda (v next) (if v (cons v (loop next)) '()))))))
(define (f) (with-continuation-mark 'a 1 (list (g))))
(f)`,
			"((#(2) #(1)))"},
		{"an iterator is pure",
			`(with-continuation-mark 'a 1 (let ((it (continuation-mark-set->iterator (current-continuation-marks) '(a)))) (list (call-with-values it (lambda (v n) v)) (call-with-values it (lambda (v n) v)))))`,
			"(#(1) #(1))"},
		{"none-v fills a missing key",
			`(with-continuation-mark 'a 1 (call-with-values (continuation-mark-set->iterator #f '(a b) 'none) (lambda (v n) v)))`,
			"#(1 none)"},
		{"exhausted answers #f and another iterator",
			`(let ((it (continuation-mark-set->iterator #f '(a)))) (call-with-values it (lambda (v n) (list v (procedure? n) (call-with-values n (lambda (v2 n2) v2))))))`,
			"(#f #t #f)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			engine, err := wile.NewEngine(context.Background(),
				wile.WithProfile(wile.KitchenSink), wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths())
			qt.Assert(t, err, qt.IsNil)
			_, err = engine.EvalMultiple(context.Background(), `(import (wile control))`)
			qt.Assert(t, err, qt.IsNil)
			result, err := engine.EvalMultiple(context.Background(), tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

// TestContinuationMarksRacketArguments pins two Racket argument forms the
// mark primitives refused: (continuation-marks #f) is the EMPTY mark set (not
// the current marks #f means to the accessors), and the accessors take
// Racket's optional trailing prompt tag, which with #f bounds the current marks
// at that tag's prompt. Expected values measured on racket.
func TestContinuationMarksRacketArguments(t *testing.T) {
	t.Parallel()

	// One expression: the unoptimized runner evaluates a single form.
	tagged := func(body string) string {
		return `(let ()
(define tag (make-continuation-prompt-tag 't))
(define (at-inner f) (with-continuation-mark 'a 1 (call-with-continuation-prompt (lambda () (with-continuation-mark 'a 2 (f))) tag (lambda a a))))
` + body + `)`
	}
	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{"continuation-marks of #f is empty", `(continuation-mark-set->list (continuation-marks #f) 'a)`, "()"},
		{"continuation-marks of #f is a mark set", `(continuation-mark-set? (continuation-marks #f))`, "#t"},
		{"first with a tag", tagged(`(at-inner (lambda () (continuation-mark-set-first #f 'a #f tag)))`), "2"},
		{"->list with a tag stops at the prompt", tagged(`(at-inner (lambda () (continuation-mark-set->list #f 'a tag)))`), "(2)"},
		{"->list* with a tag", tagged(`(at-inner (lambda () (continuation-mark-set->list* #f '(a) #f tag)))`), "(#(2))"},
		{"first with a tag stops at the prompt", tagged(`(with-continuation-mark 'a 1 (call-with-continuation-prompt (lambda () (continuation-mark-set-first #f 'a 'none tag)) tag (lambda a a)))`), "none"},
		{"control: ->list without a tag sees past the prompt", tagged(`(at-inner (lambda () (continuation-mark-set->list #f 'a)))`), "(2 1)"},
	}
	for name, run := range markShapeRunners() {
		for _, tc := range tcs {
			t.Run(name+"/"+tc.name, func(t *testing.T) {
				got, err := run(t, tc.code)
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, got, qt.Equals, tc.expected)
			})
		}
	}
}

// TestContinuationMarksRacketArgumentRefusals: a non-continuation other than
// #f stays a type error, a bad tag is ErrNotAPromptTag, and a tag with an
// explicit mark set is refused (a Wile mark set records no prompt boundaries,
// so it cannot truncate as Racket does).
func TestContinuationMarksRacketArgumentRefusals(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name string
		code string
		want error
	}{
		{"continuation-marks of a number", `(continuation-marks 5)`, werr.ErrNotAContinuation},
		{"bad tag", `(continuation-mark-set->list #f 'a 5)`, werr.ErrNotAPromptTag},
		{"tag with an explicit mark set", `(continuation-mark-set->list (current-continuation-marks) 'a (make-continuation-prompt-tag 'x))`, werr.ErrNotImplemented},
		{"continuation-marks of #f still checks its tag", `(continuation-marks #f 5)`, werr.ErrNotAPromptTag},
		{"a tag with no prompt on the continuation", `(continuation-mark-set->list #f 'a (make-continuation-prompt-tag 'x))`, werr.ErrNotAPromptTag},
		{"current-continuation-marks with a tag and no prompt", `(current-continuation-marks (make-continuation-prompt-tag 'x))`, werr.ErrNotAPromptTag},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.ErrorIs, tc.want)
		})
	}
}

// TestErrorContextMarksWithoutSnapshotIsEmptySet: an ErrorContext built with
// no marks (only a Go caller can make one) answers the empty set, not #f, which
// the accessors would read as the handler site's own marks.
func TestErrorContextMarksWithoutSnapshotIsEmptySet(t *testing.T) {
	t.Parallel()

	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background())
	c.Assert(err, qt.IsNil)
	c.Assert(engine.Define("ctx", wile.WrapValue(machine.NewErrorContext(nil, nil, nil))), qt.IsNil)
	result, err := engine.EvalMultiple(context.Background(),
		`(with-continuation-mark 'k 'handler-site (list (continuation-mark-set? (error-context-marks ctx)) (continuation-mark-set-first (error-context-marks ctx) 'k 'none)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(#t none)")
}

// TestDefaultPromptTagAcceptedInsideEval: eval runs in a sub-context with no
// prompt frame, and the missing-prompt refusal must not reach the default tag
// there (Racket's default prompt is at every continuation's root).
func TestDefaultPromptTagAcceptedInsideEval(t *testing.T) {
	t.Parallel()

	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	for _, code := range []string{
		`(eval '(with-continuation-mark 'a 1 (continuation-mark-set->list #f 'a (default-continuation-prompt-tag))) (interaction-environment))`,
		`(eval '(with-continuation-mark 'a 1 (continuation-mark-set->list (current-continuation-marks (default-continuation-prompt-tag)) 'a)) (interaction-environment))`,
	} {
		result, err := engine.EvalMultiple(context.Background(), code)
		c.Assert(err, qt.IsNil, qt.Commentf("%s", code))
		c.Assert(result.SchemeString(), qt.Equals, "(1)")
	}
}
