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

// --- A2: SRFI-13 library API completeness (string-titlecase / string-hash / xsubstring) ---

// TestSRFI13StringTitlecase pins SRFI-13 string-titlecase: the first cased
// character of each word is uppercased and the rest of the word downcased; a
// word is a maximal run of alphabetic chars; non-alphabetic chars are copied
// unchanged and end the current word. The optional [start end] range restricts
// which span is rewritten (chars outside the range are copied verbatim).
func TestSRFI13StringTitlecase(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	cases := []struct {
		name string
		expr string
		want string
	}{
		{"two-words", `(string-titlecase "hello world")`, `"Hello World"`},
		{"leading-punct-and-mixed-case", `(string-titlecase "--capitalize THIS!")`, `"--Capitalize This!"`},
		{"already-titlecased", `(string-titlecase "Hello World")`, `"Hello World"`},
		{"digits-end-word", `(string-titlecase "ab2cd")`, `"Ab2Cd"`},
		{"empty-string", `(string-titlecase "")`, `""`},
		{"start-range", `(string-titlecase "hello world" 6)`, `"hello World"`},
		{"start-end-range", `(string-titlecase "aBC dEF" 0 7)`, `"Abc Def"`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			got := evalSRFI(c, eng, `(begin (import (srfi 13)) `+tc.expr+`)`)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

// TestSRFI13StringHash pins SRFI-13 string-hash: a non-negative, deterministic
// hash; bounded into [0, bound) when bound is supplied; equal strings hash
// equal. The hash predicates are asserted (rather than the raw value) so the
// test does not over-specify the polynomial constant.
func TestSRFI13StringHash(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	cases := []struct {
		name string
		expr string
		want string
	}{
		{"non-negative", `(>= (string-hash "abc") 0)`, `#t`},
		{"bounded-below-bound", `(< (string-hash "abc" 100) 100)`, `#t`},
		{"bounded-non-negative", `(>= (string-hash "abc" 100) 0)`, `#t`},
		{"stable-within-run", `(= (string-hash "abc") (string-hash "abc"))`, `#t`},
		{"empty-string-bounded", `(string-hash "" 50)`, `0`},
		{"bound-one-collapses-to-zero", `(string-hash "anything" 1)`, `0`},
		{"range-hashes-substring", `(= (string-hash "abc" 1000 0 1) (string-hash "axx" 1000 0 1))`, `#t`},
		{"distinct-prefixes-differ", `(not (= (string-hash "abc" 1000 0 1) (string-hash "abc" 1000 1 2)))`, `#t`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			got := evalSRFI(c, eng, `(begin (import (srfi 13)) `+tc.expr+`)`)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

// TestSRFI13Xsubstring pins SRFI-13 xsubstring: the source slice s[start..end)
// is treated as an infinite cyclic string (index 0 == s[start]); indices
// [from, to) are extracted. to defaults to from + (end - start). Negative and
// out-of-range from/to wrap cyclically; from == to yields "".
func TestSRFI13Xsubstring(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	cases := []struct {
		name string
		expr string
		want string
	}{
		{"default-to-one-period", `(xsubstring "abcdef" 0)`, `"abcdef"`},
		{"extend-past-end", `(xsubstring "abc" 0 7)`, `"abcabca"`},
		{"negative-from-wraps", `(xsubstring "abc" -2 2)`, `"bcab"`},
		{"rotate-past-end", `(xsubstring "abcdef" 2 8)`, `"cdefab"`},
		{"empty-when-from-equals-to", `(xsubstring "abc" 1 1)`, `""`},
		{"start-end-slice", `(xsubstring "Hello World" 0 6 0 5)`, `"HelloH"`},
		{"negative-from-with-slice", `(xsubstring "xabcy" -1 2 1 4)`, `"cab"`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			got := evalSRFI(c, eng, `(begin (import (srfi 13)) `+tc.expr+`)`)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

// TestSRFI13XsubstringErrors pins the two error edges of xsubstring: from > to,
// and replicating an empty source slice when from /= to. Both must raise.
func TestSRFI13XsubstringErrors(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	cases := []struct {
		name string
		expr string
	}{
		{"from-greater-than-to", `(xsubstring "abc" 3 0)`},
		{"empty-slice-nonempty-request", `(xsubstring "abc" 0 3 1 1)`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			_, err := eng.EvalMultiple(context.Background(),
				`(begin (import (srfi 13)) `+tc.expr+`)`)
			c.Assert(err, qt.IsNotNil)
		})
	}
}

// NOTE: Task 6I (chibi `test` should record a raising test as a failure instead
// of aborting the suite) was REVERTED from this branch. The natural fix — wrap
// the test body in `guard` — weaponizes a pre-existing Wile VM bug: a `guard`
// around an expression that internally escapes via call/cc (e.g. char-set-every's
// short-circuit) loses the post-guard continuation, silently truncating the rest
// of the suite (process exits 0, read as PASS by the integration harness). 6I is
// blocked on fixing that guard+escaping-continuation bug. See the overnight
// roadmap's morning briefing.
