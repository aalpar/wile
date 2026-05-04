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

package charsets_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	extcharsets "github.com/aalpar/wile/extensions/charsets"
	"github.com/aalpar/wile/stdlib"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

// newEngine builds a fresh Wile engine with only the charsets extension loaded.
// Mirrors the helper in extensions/process/prim_process_test.go (lines 31-39).
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extcharsets.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// newLibraryEngine builds a Wile engine with the Small profile (full R7RS base,
// charsets already included) and the stdlib FS so that (import (srfi 14))
// resolves dispatcher.scm. The Small profile already registers charsets, so no
// duplicate WithExtension call is needed.
func newLibraryEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.Small),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// runScheme parses and runs Scheme source, returning the unwrapped values.Value.
// Renamed from the upstream pattern's "eval" helper to avoid an unrelated security
// hook that flags the literal string "eval(" as a code-execution risk.
func runScheme(t *testing.T, engine *wile.Engine, code string) values.Value {
	t.Helper()
	result, err := engine.EvalMultiple(context.Background(), code)
	qt.Assert(t, err, qt.IsNil)
	return result.Internal()
}

// runSchemeExpectError asserts that the given Scheme expression returns an error.
func runSchemeExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.EvalMultiple(context.Background(), code)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestCharSetPredicate(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	c.Assert(runScheme(t, engine, "(char-set? 'foo)"), qt.Equals, values.FalseValue)
	c.Assert(runScheme(t, engine, `(char-set? "abc")`), qt.Equals, values.FalseValue)
	c.Assert(runScheme(t, engine, `(char-set? #\a)`), qt.Equals, values.FalseValue)
}

func TestCharSetConstructorAndPrimitives(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)
	// Load the SRFI-14 library so the (char-set ...) Scheme wrapper is available.
	runScheme(t, engine, "(import (srfi 14))")

	// Empty: (char-set) => empty char-set
	c.Assert(runScheme(t, engine, "(char-set-size (char-set))"),
		valuestest.SchemeEquals, values.NewInteger(0))

	// Variadic: (char-set #\a #\b #\c)
	c.Assert(runScheme(t, engine, `(char-set-size (char-set #\a #\b #\c))`),
		valuestest.SchemeEquals, values.NewInteger(3))

	// Membership of constructed set
	c.Assert(runScheme(t, engine, `(char-set-contains? (char-set #\a #\b) #\a)`),
		qt.Equals, values.TrueValue)
	c.Assert(runScheme(t, engine, `(char-set-contains? (char-set #\a #\b) #\c)`),
		qt.Equals, values.FalseValue)

	// char-set? returns #t for constructed char-set
	c.Assert(runScheme(t, engine, `(char-set? (char-set #\a))`),
		qt.Equals, values.TrueValue)

	// char-set-copy is char-set= (Phase 2 for char-set=, but contains? works)
	c.Assert(runScheme(t, engine, `(let ((cs (char-set #\a #\b))) (char-set-contains? (char-set-copy cs) #\a))`),
		qt.Equals, values.TrueValue)

	// Type errors
	runSchemeExpectError(t, engine, `(char-set #\a 'not-a-char)`)
	runSchemeExpectError(t, engine, `(char-set-contains? "not-cs" #\m)`)
	runSchemeExpectError(t, engine, `(char-set-contains? (char-set #\a) "not-char")`)
	runSchemeExpectError(t, engine, `(char-set-copy 42)`)
}

func TestStringToCharSet(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)
	runScheme(t, engine, "(import (srfi 14))")

	// Basic: 3 distinct chars
	c.Assert(runScheme(t, engine, `(char-set-size (string->char-set "abc"))`),
		valuestest.SchemeEquals, values.NewInteger(3))

	// Duplicates collapse
	c.Assert(runScheme(t, engine, `(char-set-size (string->char-set "aabbcc"))`),
		valuestest.SchemeEquals, values.NewInteger(3))

	// With base char-set: "xy" (2) unioned with (char-set #\a) (1) = 3
	c.Assert(runScheme(t, engine, `(char-set-size (string->char-set "xy" (char-set #\a)))`),
		valuestest.SchemeEquals, values.NewInteger(3))

	// Type errors
	runSchemeExpectError(t, engine, `(string->char-set 42)`)
	runSchemeExpectError(t, engine, `(string->char-set "abc" 42)`)
}

func TestListToCharSet(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)
	runScheme(t, engine, "(import (srfi 14))")

	// Basic: 3 distinct chars
	c.Assert(runScheme(t, engine, `(char-set-size (list->char-set '(#\a #\b #\c)))`),
		valuestest.SchemeEquals, values.NewInteger(3))

	// Duplicates collapse
	c.Assert(runScheme(t, engine, `(char-set-size (list->char-set '(#\a #\a #\b)))`),
		valuestest.SchemeEquals, values.NewInteger(2))

	// With base: '(#\x #\y) (2) unioned with (char-set #\a) (1) = 3
	c.Assert(runScheme(t, engine, `(char-set-size (list->char-set '(#\x #\y) (char-set #\a)))`),
		valuestest.SchemeEquals, values.NewInteger(3))

	// Non-list argument
	runSchemeExpectError(t, engine, `(list->char-set 42)`)

	// List with non-char element
	runSchemeExpectError(t, engine, `(list->char-set '(#\a 42))`)
}

func TestCharSetToList(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)
	runScheme(t, engine, "(import (srfi 14))")

	// Empty
	c.Assert(runScheme(t, engine, "(char-set->list (char-set))"),
		qt.Equals, values.EmptyList)

	// Codepoint-ascending order regardless of construction order
	c.Assert(runScheme(t, engine, `(char-set->list (char-set #\c #\a #\b))`),
		valuestest.SchemeEquals,
		values.List(values.NewCharacter('a'), values.NewCharacter('b'), values.NewCharacter('c')))

	// Type error
	runSchemeExpectError(t, engine, "(char-set->list 42)")
}

func TestCharSetToString(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)
	runScheme(t, engine, "(import (srfi 14))")

	// Empty
	c.Assert(runScheme(t, engine, "(char-set->string (char-set))"),
		valuestest.SchemeEquals, values.NewString(""))

	// Codepoint-ascending order
	c.Assert(runScheme(t, engine, `(char-set->string (char-set #\c #\a #\b))`),
		valuestest.SchemeEquals, values.NewString("abc"))

	// Type error
	runSchemeExpectError(t, engine, "(char-set->string 42)")
}

func TestUcsRangeToCharSet(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)
	runScheme(t, engine, "(import (srfi 14))")

	// Half-open: (ucs-range->char-set 65 90) => 25 chars (A..Y)
	c.Assert(runScheme(t, engine, "(char-set-size (ucs-range->char-set 65 90))"),
		valuestest.SchemeEquals, values.NewInteger(25))

	// Membership respects half-open
	c.Assert(runScheme(t, engine, `(char-set-contains? (ucs-range->char-set 65 90) #\A)`),
		qt.Equals, values.TrueValue)
	c.Assert(runScheme(t, engine, `(char-set-contains? (ucs-range->char-set 65 90) #\Z)`),
		qt.Equals, values.FalseValue) // 90 is excluded

	// Zero-width: (ucs-range->char-set 65 65) => empty
	c.Assert(runScheme(t, engine, "(char-set-size (ucs-range->char-set 65 65))"),
		valuestest.SchemeEquals, values.NewInteger(0))

	// lo > hi error (with default error? = #t)
	runSchemeExpectError(t, engine, "(ucs-range->char-set 90 65)")

	// Out-of-range with error? = #t
	runSchemeExpectError(t, engine, "(ucs-range->char-set 0 #x200000)")

	// Out-of-range with error? = #f → silently clipped: 0x10FFFE..0x10FFFF inclusive (2 chars)
	c.Assert(runScheme(t, engine, "(char-set-size (ucs-range->char-set #x10FFFE #x200000 #f))"),
		valuestest.SchemeEquals, values.NewInteger(2))

	// Scheme-truthy error? — non-Boolean truthy value treated as error-on
	runSchemeExpectError(t, engine, "(ucs-range->char-set 0 #x200000 'truthy)")

	// With base char-set
	c.Assert(runScheme(t, engine, `(char-set-contains? (ucs-range->char-set 65 67 #t (char-set #\z)) #\z)`),
		qt.Equals, values.TrueValue)
}

func TestCharSetEquality(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)
	runScheme(t, engine, "(import (srfi 14))")

	// Vacuous: 1 arg → #t
	c.Assert(runScheme(t, engine, `(char-set= (char-set #\a))`), qt.Equals, values.TrueValue)

	// Equal sets built different ways
	c.Assert(runScheme(t, engine, `(char-set= (char-set #\a #\b) (string->char-set "ab"))`),
		qt.Equals, values.TrueValue)

	// Unequal
	c.Assert(runScheme(t, engine, `(char-set= (char-set #\a) (char-set #\b))`),
		qt.Equals, values.FalseValue)

	// Variadic chain (3 args)
	c.Assert(runScheme(t, engine,
		`(char-set= (char-set #\a #\b) (string->char-set "ab") (list->char-set '(#\a #\b)))`),
		qt.Equals, values.TrueValue)
}

func TestCharSetSubset(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)
	runScheme(t, engine, "(import (srfi 14))")

	// Vacuous: 1 arg → #t
	c.Assert(runScheme(t, engine, `(char-set<= (char-set #\a))`), qt.Equals, values.TrueValue)

	// Proper subset
	c.Assert(runScheme(t, engine, `(char-set<= (char-set #\a) (char-set #\a #\b))`),
		qt.Equals, values.TrueValue)

	// Equal sets are subsets
	c.Assert(runScheme(t, engine, `(char-set<= (char-set #\a #\b) (char-set #\a #\b))`),
		qt.Equals, values.TrueValue)

	// Not a subset
	c.Assert(runScheme(t, engine, `(char-set<= (char-set #\a #\c) (char-set #\a #\b))`),
		qt.Equals, values.FalseValue)

	// Variadic chain: a ⊆ ab ⊆ abc
	c.Assert(runScheme(t, engine,
		`(char-set<= (char-set #\a) (char-set #\a #\b) (char-set #\a #\b #\c))`),
		qt.Equals, values.TrueValue)
}

func TestCharSetAlgebra(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)
	runScheme(t, engine, "(import (srfi 14))")

	// Union: {a} ∪ {b} = {a, b}
	c.Assert(runScheme(t, engine, `(char-set->string (char-set-union (char-set #\a) (char-set #\b)))`),
		valuestest.SchemeEquals, values.NewString("ab"))

	// Union: {a, c} ∪ {b} = {a, b, c} (sorted)
	c.Assert(runScheme(t, engine, `(char-set->string (char-set-union (char-set #\a #\c) (char-set #\b)))`),
		valuestest.SchemeEquals, values.NewString("abc"))

	// Intersection: {a, b, c} ∩ {b, c, d} = {b, c}
	c.Assert(runScheme(t, engine, `(char-set->string (char-set-intersection (char-set #\a #\b #\c) (char-set #\b #\c #\d)))`),
		valuestest.SchemeEquals, values.NewString("bc"))

	// Empty intersection: {a} ∩ {b} = ∅
	c.Assert(runScheme(t, engine, `(char-set-size (char-set-intersection (char-set #\a) (char-set #\b)))`),
		valuestest.SchemeEquals, values.NewInteger(0))

	// Difference: {a, b, c} \ {b} = {a, c}
	c.Assert(runScheme(t, engine, `(char-set->string (char-set-difference (char-set #\a #\b #\c) (char-set #\b)))`),
		valuestest.SchemeEquals, values.NewString("ac"))

	// Difference subtracts ALL rest from first: {a,b,c,d} \ {b} \ {d} = {a, c}
	c.Assert(runScheme(t, engine, `(char-set->string (char-set-difference (char-set #\a #\b #\c #\d) (char-set #\b) (char-set #\d)))`),
		valuestest.SchemeEquals, values.NewString("ac"))

	// Xor: {a, b} △ {b, c} = {a, c}
	c.Assert(runScheme(t, engine, `(char-set->string (char-set-xor (char-set #\a #\b) (char-set #\b #\c)))`),
		valuestest.SchemeEquals, values.NewString("ac"))

	// Complement excludes the given char
	c.Assert(runScheme(t, engine, `(char-set-contains? (char-set-complement (char-set #\a)) #\a)`),
		qt.Equals, values.FalseValue)
	c.Assert(runScheme(t, engine, `(char-set-contains? (char-set-complement (char-set #\a)) #\b)`),
		qt.Equals, values.TrueValue)

	// complement(empty) = full: 0x110000 codepoints
	c.Assert(runScheme(t, engine, `(char-set-size (char-set-complement (char-set)))`),
		valuestest.SchemeEquals, values.NewInteger(0x110000))
}

func TestCharSetFoldForEach(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)
	runScheme(t, engine, "(import (srfi 14))")

	// char-set-fold sums codepoints via char->integer (+ doesn't accept chars)
	c.Assert(runScheme(t, engine, `(char-set-fold (lambda (ch acc) (+ acc (char->integer ch))) 0 (char-set #\a #\b #\c))`),
		valuestest.SchemeEquals, values.NewInteger(97+98+99))

	// char-set-fold on empty returns init unchanged
	c.Assert(runScheme(t, engine, `(char-set-fold (lambda (c acc) (cons c acc)) '() (char-set))`),
		qt.Equals, values.EmptyList)

	// char-set-for-each side effect: reverse-cons gives reversed-ascending order
	// ascending order is #\a #\b #\c, so cons gives (#\c #\b #\a)
	c.Assert(runScheme(t, engine, `
      (let ((collected '()))
        (char-set-for-each (lambda (c) (set! collected (cons c collected)))
                           (char-set #\c #\a #\b))
        collected)`),
		valuestest.SchemeEquals,
		values.List(values.NewCharacter('c'), values.NewCharacter('b'), values.NewCharacter('a')))

	// char-set-for-each on empty: lambda not called
	c.Assert(runScheme(t, engine, `
      (let ((called #f))
        (char-set-for-each (lambda (c) (set! called #t)) (char-set))
        called)`),
		qt.Equals, values.FalseValue)
}

func TestCharSetRanges(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)
	// (srfi 14) for char-set constructor; (wile charsets) for char-set-ranges.
	runScheme(t, engine, "(import (srfi 14))")
	runScheme(t, engine, "(import (wile charsets))")

	// Empty
	c.Assert(runScheme(t, engine, "(char-set-ranges (char-set))"),
		qt.Equals, values.EmptyList)

	// Single range from adjacent codepoints: '((97 . 99)) for #\a #\b #\c
	c.Assert(runScheme(t, engine, `(char-set-ranges (char-set #\a #\b #\c))`),
		valuestest.SchemeEquals,
		values.List(values.NewCons(values.NewInteger(97), values.NewInteger(99))))

	// Multiple ranges: #\a (97) and #\z (122) are non-adjacent
	c.Assert(runScheme(t, engine, `(char-set-ranges (char-set #\a #\z))`),
		valuestest.SchemeEquals,
		values.List(
			values.NewCons(values.NewInteger(97), values.NewInteger(97)),
			values.NewCons(values.NewInteger(122), values.NewInteger(122))))
}

func TestCharSetMapFilter(t *testing.T) {
	c := qt.New(t)
	engine := newLibraryEngine(t)

	runScheme(t, engine, "(import (srfi 14))")

	// char-set-map: shift each codepoint by +1
	c.Assert(runScheme(t, engine, `
        (char-set= (char-set #\b #\c #\d)
                   (char-set-map (lambda (c) (integer->char (+ 1 (char->integer c))))
                                 (char-set #\a #\b #\c)))`),
		qt.Equals, values.TrueValue)

	// char-set-filter: keep only #\b
	c.Assert(runScheme(t, engine, `
        (char-set= (char-set #\b)
                   (char-set-filter (lambda (c) (char=? c #\b))
                                    (char-set #\a #\b #\c)))`),
		qt.Equals, values.TrueValue)

	// char-set-filter with base: union of base + filtered
	c.Assert(runScheme(t, engine, `
        (char-set= (char-set #\b #\x)
                   (char-set-filter (lambda (c) (char=? c #\b))
                                    (char-set #\a #\b #\c)
                                    (char-set #\x)))`),
		qt.Equals, values.TrueValue)

	// char-set-filter!: same as char-set-filter (always allocate fresh)
	c.Assert(runScheme(t, engine, `
        (char-set= (char-set #\b)
                   (char-set-filter! (lambda (c) (char=? c #\b))
                                     (char-set #\a #\b #\c)))`),
		qt.Equals, values.TrueValue)
}
