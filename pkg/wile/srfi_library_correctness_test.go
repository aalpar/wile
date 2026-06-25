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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
)

// newSRFITestEngine builds a KitchenSink engine wired so the embedded stdlib
// libraries (SRFI-1/13/14, chibi test) resolve: WithSourceFS(stdlib.FS) supplies
// the embedded library tree, WithLibraryPaths() registers the default search
// paths, and WithSourceOS() appends the OS filesystem as a fallback resolver.
func newSRFITestEngine(t *testing.T) *Engine {
	t.Helper()
	eng, err := NewEngine(context.Background(),
		WithProfile(KitchenSink),
		WithLibraryPaths(),
		WithSourceFS(stdlib.FS),
		WithSourceOS(),
	)
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// evalSRFI evaluates program and returns its printed value, failing the test on
// any error. Programs are (begin ...)-wrapped where forward references between
// defines matter; callers that only import + call a single expression need not
// wrap.
func evalSRFI(t testing.TB, eng *Engine, program string) string {
	t.Helper()
	result, err := eng.EvalMultiple(context.Background(), program)
	qt.Assert(t, err, qt.IsNil, qt.Commentf("eval failed for: %s", program))
	return result.SchemeString()
}

// --- 6A: SRFI-1 reduce-right folds the LAST element as the base ---

// TestSRFI1ReduceRight pins the SRFI-1 semantics: for a non-empty list,
// reduce-right folds right-to-left using the LAST element as the base (not
// ridentity). ridentity is only the result for the empty list.
func TestSRFI1ReduceRight(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	c.Run("nested-list", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 1)) (reduce-right list '() '(1 2 3)))`)
		c.Assert(got, qt.Equals, "(1 (2 3))")
	})
	c.Run("cons-dotted-tail", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 1)) (reduce-right cons '() '(1 2 3)))`)
		c.Assert(got, qt.Equals, "(1 2 . 3)")
	})
	c.Run("singleton-returns-element", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 1)) (reduce-right list '() '(5)))`)
		c.Assert(got, qt.Equals, "5")
	})
	c.Run("empty-returns-ridentity", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 1)) (reduce-right + 0 '()))`)
		c.Assert(got, qt.Equals, "0")
	})
}

// --- 6B: SRFI-1 last / last-pair handle dotted lists ---

// TestSRFI1LastDotted pins that last/last-pair terminate on a non-pair cdr (not
// only on a null cdr), so they handle improper (dotted) lists per SRFI-1 instead
// of crashing with "cdr: not a pair".
func TestSRFI1LastDotted(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	c.Run("last-dotted", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 1)) (last '(a b . c)))`)
		c.Assert(got, qt.Equals, "b")
	})
	c.Run("last-pair-dotted", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 1)) (last-pair '(a b . c)))`)
		c.Assert(got, qt.Equals, "(b . c)")
	})
	c.Run("last-proper-unchanged", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 1)) (last '(a b c)))`)
		c.Assert(got, qt.Equals, "c")
	})
	c.Run("last-pair-proper-unchanged", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 1)) (last-pair '(a b c)))`)
		c.Assert(got, qt.Equals, "(c)")
	})
}

// --- 6C: SRFI-1 delete-duplicates uses SRFI argument order ---

// TestSRFI1DeleteDuplicatesArgOrder pins the SRFI-1 reference semantics: the
// equality predicate is called (= earlier-elt later-elt), and duplicates are
// removed from the tail. For symmetric predicates (equal?) the order is
// invisible; for an asymmetric predicate like < the order is observable —
// (delete-duplicates '(1 5 2 9 3) <) is (1) per the SRFI-1 reference, because
// every later element y satisfies (< 1 y) and is deleted.
func TestSRFI1DeleteDuplicatesArgOrder(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	c.Run("asymmetric-predicate", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 1)) (delete-duplicates '(1 5 2 9 3) <))`)
		c.Assert(got, qt.Equals, "(1)")
	})
	c.Run("default-equal-unchanged", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 1)) (delete-duplicates '(1 2 1 3 2 4)))`)
		c.Assert(got, qt.Equals, "(1 2 3 4)")
	})
	c.Run("symbols-unchanged", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 1)) (delete-duplicates '(a a b b c)))`)
		c.Assert(got, qt.Equals, "(a b c)")
	})
}

// --- 6E: SRFI-14 char-set-unfold argument order ---

// TestSRFI14CharSetUnfoldArgOrder pins the SRFI-14 argument order
// (char-set-unfold f p g seed [base]) = (mapper stop? successor seed). A
// spec-order call that emits chars A,B,C (codepoints 65..67) must yield a set of
// size 3; before the fix the transposed parameters silently produced the empty
// set.
func TestSRFI14CharSetUnfoldArgOrder(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	c.Run("spec-order-size", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 14))
(char-set-size (char-set-unfold integer->char (lambda (i) (> i 67)) (lambda (i) (+ i 1)) 65)))`)
		c.Assert(got, qt.Equals, "3")
	})
	c.Run("spec-order-membership", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 14))
(let ((cs (char-set-unfold integer->char (lambda (i) (> i 67)) (lambda (i) (+ i 1)) 65)))
  (list (char-set-contains? cs #\A)
        (char-set-contains? cs #\B)
        (char-set-contains? cs #\C)
        (char-set-contains? cs #\D))))`)
		c.Assert(got, qt.Equals, "(#t #t #t #f)")
	})
}

// --- 6H: SRFI-13 default delimiter / tokenize criterion ---

// TestSRFI13StringJoinDefaultDelimiter pins the SRFI-13 default delimiter for
// string-join: a single space, not the empty string.
func TestSRFI13StringJoinDefaultDelimiter(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	c.Run("default-space", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 13)) (string-join '("a" "b" "c")))`)
		c.Assert(got, qt.Equals, `"a b c"`)
	})
	c.Run("explicit-delim-unchanged", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 13)) (string-join '("a" "b" "c") ","))`)
		c.Assert(got, qt.Equals, `"a,b,c"`)
	})
	c.Run("empty-list-unchanged", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 13)) (string-join '()))`)
		c.Assert(got, qt.Equals, `""`)
	})
}

// TestSRFI13StringTokenizeDefaultCriterion pins the SRFI-13 default criterion
// for string-tokenize: char-set:graphic. The discriminating case is a control
// char that is NOT whitespace (SOH, codepoint 1): under the old default
// (not (char-whitespace? ch)) it was pulled INTO a token; under char-set:graphic
// it is a separator. The tab case (whitespace) split under both defaults and is
// kept as a regression guard.
func TestSRFI13StringTokenizeDefaultCriterion(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	c.Run("non-whitespace-control-char-separates", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 13))
(string-tokenize (string #\a (integer->char 1) #\b)))`)
		c.Assert(got, qt.Equals, `("a" "b")`)
	})
	c.Run("tab-still-separates", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 13)) (string-tokenize "a\tb"))`)
		c.Assert(got, qt.Equals, `("a" "b")`)
	})
	c.Run("whitespace-runs-unchanged", func(c *qt.C) {
		got := evalSRFI(c, eng, `(begin (import (srfi 13)) (string-tokenize "  many   spaces  "))`)
		c.Assert(got, qt.Equals, `("many" "spaces")`)
	})
}

// --- 6I: chibi test records a raising test as a failure, not a suite abort ---

// TestChibiTestRaisingExpressionIsFailure pins that a test expression that raises
// is recorded as one failure and execution continues: a test-group containing a
// raising (test ...) followed by a passing (test ...) must run the second test
// too, ending with 1 fail + 1 pass. The probe measures the failure-count delta
// across the group; before the fix the raise aborted the group (and the eval)
// so the second test never ran.
func TestChibiTestRaisingExpressionIsFailure(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	program := `(begin
(import (chibi test))
(define (fail-count) ((test-failure-count)))
(define start-fail (fail-count))
(test-group "raise-then-pass"
  (test 1 (raise 'boom))
  (test 2 (+ 1 1)))
(define end-fail (fail-count))
;; one new failure recorded (the raising test); the second test still ran and
;; passed, adding no failure. Verify exactly one failure was added.
(- end-fail start-fail))`
	got := evalSRFI(c, eng, program)
	c.Assert(got, qt.Equals, "1")
}
