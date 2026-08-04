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
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
)

func TestHashtable_MakeAndPredicate(t *testing.T) {
	c := qt.New(t)
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "make-hashtable returns hashtable", Code: `(hashtable? (make-equal-hashtable))`, Expected: values.TrueValue},
		{Name: "integer is not hashtable", Code: `(hashtable? 42)`, Expected: values.FalseValue},
		{Name: "string is not hashtable", Code: `(hashtable? "hello")`, Expected: values.FalseValue},
		{Name: "pair is not hashtable", Code: `(hashtable? '(1 2))`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestHashtable_SetAndRef(t *testing.T) {
	c := qt.New(t)
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "set and ref round-trip", Code: `(let ((ht (make-equal-hashtable)))
			   (hashtable-set! ht 'key 42)
			   (hashtable-ref ht 'key #f))`, Expected: values.NewInteger(42)},
		{Name: "ref with default on missing key", Code: `(let ((ht (make-equal-hashtable)))
			   (hashtable-ref ht 'missing 99))`, Expected: values.NewInteger(99)},
		{Name: "integer key", Code: `(let ((ht (make-equal-hashtable)))
			   (hashtable-set! ht 1 "one")
			   (hashtable-ref ht 1 #f))`, Expected: values.NewString("one")},
		{Name: "string key", Code: `(let ((ht (make-equal-hashtable)))
			   (hashtable-set! ht "key" 100)
			   (hashtable-ref ht "key" #f))`, Expected: values.NewInteger(100)},
		{Name: "boolean key", Code: `(let ((ht (make-equal-hashtable)))
			   (hashtable-set! ht #t "yes")
			   (hashtable-ref ht #t #f))`, Expected: values.NewString("yes")},
		{Name: "overwrite existing key", Code: `(let ((ht (make-equal-hashtable)))
			   (hashtable-set! ht 'key 1)
			   (hashtable-set! ht 'key 2)
			   (hashtable-ref ht 'key #f))`, Expected: values.NewInteger(2)},
	}
	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestHashtable_RefRequiresDefault replaces TestHashtable_RefErrorOnMissing.
//
// The old test asserted that a two-argument hashtable-ref RAISES on a missing
// key. R6RS has no two-argument form: DEFAULT is required, and an absent key
// returns it rather than erroring. So the arity is now the error and the missing
// key is not — werr.ErrHashtableKeyNotFound went dead with the change.
func TestHashtable_RefRequiresDefault(t *testing.T) {
	t.Run("two-argument form is an arity error", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(let ((ht (make-equal-hashtable)))
			   (hashtable-ref ht 'missing))`)
	})
	t.Run("missing key returns the default, it does not raise", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(hashtable-ref (make-equal-hashtable) 'missing 'fallback)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("fallback"))
	})
}

func TestHashtable_Delete(t *testing.T) {
	c := qt.New(t)
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "delete removes key", Code: `(let ((ht (make-equal-hashtable)))
			   (hashtable-set! ht 'key 42)
			   (hashtable-delete! ht 'key)
			   (hashtable-size ht))`, Expected: values.NewInteger(0)},
		{Name: "delete non-existent key is no-op", Code: `(let ((ht (make-equal-hashtable)))
			   (hashtable-delete! ht 'missing)
			   (hashtable-size ht))`, Expected: values.NewInteger(0)},
	}
	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestHashtable_Size(t *testing.T) {
	c := qt.New(t)
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "empty hashtable size", Code: `(hashtable-size (make-equal-hashtable))`, Expected: values.NewInteger(0)},
		{Name: "size after inserts", Code: `(let ((ht (make-equal-hashtable)))
			   (hashtable-set! ht 'a 1)
			   (hashtable-set! ht 'b 2)
			   (hashtable-set! ht 'c 3)
			   (hashtable-size ht))`, Expected: values.NewInteger(3)},
	}
	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestHashtable_KeysAndValues(t *testing.T) {
	c := qt.New(t)

	// Keys returns a VECTOR now, not a list.
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((ht (make-equal-hashtable)))
		  (hashtable-set! ht 'only 1)
		  (vector-ref (hashtable-keys ht) 0))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewSymbol("only"))

	// hashtable-values is GONE — hashtable-entries subsumes it, and is the only
	// way to get keys and values paired reliably.
	result, err = testhelpers.RunSchemeCode(t, `
		(let ((ht (make-equal-hashtable)))
		  (hashtable-set! ht 'only 99)
		  (call-with-values (lambda () (hashtable-entries ht))
		    (lambda (ks vs) (vector-ref vs 0))))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(99))

	// Empty hashtable returns empty vectors.
	result, err = testhelpers.RunSchemeCode(t, `(vector-length (hashtable-keys (make-equal-hashtable)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(0))

	_, err = testhelpers.RunSchemeCode(t, `(hashtable-values (make-equal-hashtable))`)
	c.Assert(err, qt.IsNotNil, qt.Commentf("hashtable-values must be unbound"))
}

func TestHashtable_Copy(t *testing.T) {
	c := qt.New(t)

	// Copy is independent
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((ht (make-equal-hashtable)))
		  (hashtable-set! ht 'a 1)
		  (let ((cp (hashtable-copy ht)))
		    (hashtable-set! ht 'b 2)
		    (hashtable-size cp)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(1))

	// Copy preserves content
	result, err = testhelpers.RunSchemeCode(t, `
		(let ((ht (make-equal-hashtable)))
		  (hashtable-set! ht 'a 1)
		  (let ((cp (hashtable-copy ht)))
		    (hashtable-ref cp 'a #f)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(1))
}

func TestHashtable_Clear(t *testing.T) {
	c := qt.New(t)
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((ht (make-equal-hashtable)))
		  (hashtable-set! ht 'a 1)
		  (hashtable-set! ht 'b 2)
		  (hashtable-clear! ht)
		  (hashtable-size ht))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(0))
}

func TestHashtable_EqualQ(t *testing.T) {
	c := qt.New(t)

	// equal? on identical content
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((a (make-equal-hashtable))
		      (b (make-equal-hashtable)))
		  (hashtable-set! a 'x 1)
		  (hashtable-set! b 'x 1)
		  (equal? a b))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.TrueValue)

	// equal? on different content
	result, err = testhelpers.RunSchemeCode(t, `
		(let ((a (make-equal-hashtable))
		      (b (make-equal-hashtable)))
		  (hashtable-set! a 'x 1)
		  (hashtable-set! b 'x 2)
		  (equal? a b))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.FalseValue)
}

// TestHashtable_EqualQGate is the Scheme-level pin on r7rs-differences item 14.
// The Go-level gate is covered by TestHashtableEqualGate; this is the assertion
// a user reading the documented deviation would go looking for.
func TestHashtable_EqualQGate(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "same kind, leaf keys, equal contents", Code: `(let ((a (make-equal-hashtable)) (b (make-equal-hashtable))) (hashtable-set! a 'x 1) (hashtable-set! b 'x 1) (equal? a b))`, Expected: values.TrueValue},
		// Different key equivalences are not comparable entry-by-entry at all.
		{Name: "different kinds are never equal?", Code: `(equal? (make-eq-hashtable) (make-equal-hashtable))`, Expected: values.FalseValue},
		// CONTAINER keys degrade to identity — the documented cost of keeping
		// the structural-equal? deviation.
		{Name: "container keys degrade to identity", Code: `(let ((a (make-equal-hashtable)) (b (make-equal-hashtable))) (hashtable-set! a (list 1) 1) (hashtable-set! b (list 1) 1) (equal? a b))`, Expected: values.FalseValue},
		{Name: "a container-keyed table is still equal? to itself", Code: `(let ((a (make-equal-hashtable))) (hashtable-set! a (list 1) 1) (equal? a a))`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestHashtable_TypeErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "hashtable-ref on non-hashtable", Code: `(hashtable-ref 42 'key)`},
		{Name: "hashtable-set! on non-hashtable", Code: `(hashtable-set! 42 'key 1)`},
		{Name: "hashtable-delete! on non-hashtable", Code: `(hashtable-delete! 42 'key)`},
		{Name: "hashtable-keys on non-hashtable", Code: `(hashtable-keys 42)`},
		{Name: "hashtable-values on non-hashtable", Code: `(hashtable-values 42)`},
		{Name: "hashtable-size on non-hashtable", Code: `(hashtable-size 42)`},
		{Name: "hashtable-copy on non-hashtable", Code: `(hashtable-copy 42)`},
		{Name: "hashtable-clear! on non-hashtable", Code: `(hashtable-clear! 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// TestHashtable_ContainerKeyRoundTrips replaces TestHashtable_NonComparableKeyError,
// which asserted that a pair key RAISES. That was true while the key carried the
// hash and Set type-asserted Hashable; HashtableKind moved the hash to the table,
// so every kind now admits every key and the raise is gone.
//
// The old test is not merely stale, it was becoming MISLEADING: its second case
// — (hashtable-ref ht '(1 2)) with no default — still raises, but now because the
// key is ABSENT rather than because it is inadmissible. Keeping it would have
// looked like key-admission coverage while asserting something else entirely.
func TestHashtable_ContainerKeyRoundTrips(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "pair key stores and reads back", Code: `(let ((ht (make-equal-hashtable))) (hashtable-set! ht '(1 2) "val") (hashtable-ref ht '(1 2) #f))`, Expected: values.NewString("val")},
		{Name: "a distinct but equal? pair is the same key", Code: `(let ((ht (make-equal-hashtable))) (hashtable-set! ht (list 1 2) 'a) (hashtable-set! ht (list 1 2) 'b) (hashtable-size ht))`, Expected: values.NewInteger(1)},
		{Name: "vector key round-trips", Code: `(let ((ht (make-equal-hashtable))) (hashtable-set! ht (vector 1 2) 'v) (hashtable-ref ht (vector 1 2) #f))`, Expected: values.NewSymbol("v")},
		{Name: "absent container key returns the default", Code: `(hashtable-ref (make-equal-hashtable) '(1 2) 'missing)`, Expected: values.NewSymbol("missing")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestHashtableConstructors pins the three fixed-kind R6RS constructors: which
// objects count as ONE key is now the TABLE's choice, not the key's.
func TestHashtableConstructors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "eq: same-named symbols collapse", Code: `(let ((h (make-eq-hashtable))) (hashtable-set! h 'a 1) (hashtable-set! h 'a 2) (hashtable-size h))`, Expected: values.NewInteger(1)},
		{Name: "eq: equal lists stay distinct", Code: `(let ((h (make-eq-hashtable))) (hashtable-set! h (list 1) 1) (hashtable-set! h (list 1) 2) (hashtable-size h))`, Expected: values.NewInteger(2)},
		{Name: "eqv: exact representations collapse", Code: `(let ((h (make-eqv-hashtable))) (hashtable-set! h 5 1) (hashtable-set! h 5 2) (hashtable-size h))`, Expected: values.NewInteger(1)},
		{Name: "eqv: equal lists stay distinct", Code: `(let ((h (make-eqv-hashtable))) (hashtable-set! h (list 1) 1) (hashtable-set! h (list 1) 2) (hashtable-size h))`, Expected: values.NewInteger(2)},
		{Name: "equal: equal lists collapse", Code: `(let ((h (make-equal-hashtable))) (hashtable-set! h (list 1) 1) (hashtable-set! h (list 1) 2) (hashtable-size h))`, Expected: values.NewInteger(1)},
		{Name: "equal: vector key round-trips", Code: `(let ((h (make-equal-hashtable))) (hashtable-set! h (vector 1 2) 'v) (hashtable-ref h (vector 1 2) #f))`, Expected: values.NewSymbol("v")},
		{Name: "equal: pair key round-trips", Code: `(let ((h (make-equal-hashtable))) (hashtable-set! h '(a . b) 'p) (hashtable-ref h (cons 'a 'b) #f))`, Expected: values.NewSymbol("p")},
		// R6RS: "k ... is a hint ... implementations are free to ignore it."
		// Wile ignores it — the backing sync.Map has no capacity knob.
		{Name: "constructors accept a size hint", Code: `(hashtable-size (make-eq-hashtable 64))`, Expected: values.NewInteger(0)},
		{Name: "eqv accepts a size hint", Code: `(hashtable-size (make-eqv-hashtable 64))`, Expected: values.NewInteger(0)},
		{Name: "equal accepts a size hint", Code: `(hashtable-size (make-equal-hashtable 64))`, Expected: values.NewInteger(0)},
		// Copy must carry the kind, or a copied eq table would silently start
		// collapsing structurally-equal keys.
		{Name: "copy preserves the eq kind", Code: `(let ((h (hashtable-copy (make-eq-hashtable)))) (hashtable? h))`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestHashtableImmutability(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "fresh table is mutable", Code: `(hashtable-mutable? (make-equal-hashtable))`, Expected: values.TrueValue},
		{Name: "copy without flag is immutable", Code: `(hashtable-mutable? (hashtable-copy (make-equal-hashtable)))`, Expected: values.FalseValue},
		{Name: "copy with #f is immutable", Code: `(hashtable-mutable? (hashtable-copy (make-equal-hashtable) #f))`, Expected: values.FalseValue},
		{Name: "copy with #t is mutable", Code: `(hashtable-mutable? (hashtable-copy (make-equal-hashtable) #t))`, Expected: values.TrueValue},
		{Name: "immutable copy keeps contents", Code: `(let* ((h (make-equal-hashtable)) (_ (hashtable-set! h 'a 1))) (hashtable-ref (hashtable-copy h) 'a #f))`, Expected: values.NewInteger(1)},
		{Name: "mutable copy keeps kind", Code: `(let ((h (hashtable-copy (make-eq-hashtable) #t))) (hashtable-set! h (list 1) 1) (hashtable-set! h (list 1) 2) (hashtable-size h))`, Expected: values.NewInteger(2)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestHashtableImmutabilityErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "set! on immutable", Code: `(hashtable-set! (hashtable-copy (make-equal-hashtable)) 'a 1)`},
		{Name: "delete! on immutable", Code: `(hashtable-delete! (hashtable-copy (make-equal-hashtable)) 'a)`},
		{Name: "clear! on immutable", Code: `(hashtable-clear! (hashtable-copy (make-equal-hashtable)))`},
		// update! is the surface's main mutation entry point and, unlike the
		// other three, is a SCHEME bootstrap define whose error crosses a
		// compiled-procedure boundary before surfacing. A change to how
		// bootstrap-procedure errors are wrapped could swallow the sentinel
		// without this row noticing.
		{Name: "update! on immutable", Code: `(hashtable-update! (hashtable-copy (make-equal-hashtable)) 'a (lambda (v) v) 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrImmutableHashtable), qt.IsTrue,
				qt.Commentf("must match the sentinel with errors.Is, got %v", err))
		})
	}
}

func TestHashtableR6RSAccessors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "contains? finds a key", Code: `(let ((h (make-equal-hashtable))) (hashtable-set! h 'a 1) (hashtable-contains? h 'a))`, Expected: values.TrueValue},
		{Name: "contains? misses", Code: `(hashtable-contains? (make-equal-hashtable) 'a)`, Expected: values.FalseValue},
		{Name: "entries returns two values", Code: `(let ((h (make-equal-hashtable))) (hashtable-set! h 'a 1) (call-with-values (lambda () (hashtable-entries h)) (lambda (ks vs) (and (vector? ks) (vector? vs)))))`, Expected: values.TrueValue},
		{Name: "entries pairs by position", Code: `(let ((h (make-equal-hashtable))) (hashtable-set! h 'a 7) (call-with-values (lambda () (hashtable-entries h)) (lambda (ks vs) (and (eq? (vector-ref ks 0) 'a) (= (vector-ref vs 0) 7)))))`, Expected: values.TrueValue},
		{Name: "entries on empty table", Code: `(call-with-values (lambda () (hashtable-entries (make-equal-hashtable))) (lambda (ks vs) (+ (vector-length ks) (vector-length vs))))`, Expected: values.NewInteger(0)},
		{Name: "equivalence-function of equal table", Code: `(eq? (hashtable-equivalence-function (make-equal-hashtable)) equal?)`, Expected: values.TrueValue},
		{Name: "equivalence-function of eq table", Code: `(eq? (hashtable-equivalence-function (make-eq-hashtable)) eq?)`, Expected: values.TrueValue},
		{Name: "equivalence-function of eqv table", Code: `(eq? (hashtable-equivalence-function (make-eqv-hashtable)) eqv?)`, Expected: values.TrueValue},
		{Name: "hash-function of equal table", Code: `(eq? (hashtable-hash-function (make-equal-hashtable)) equal-hash)`, Expected: values.TrueValue},
		// R6RS returns #f from hashtable-hash-function for eq and eqv tables;
		// that is the spec, not a shortcut.
		{Name: "hash-function of eq table is #f", Code: `(hashtable-hash-function (make-eq-hashtable))`, Expected: values.FalseValue},
		{Name: "hash-function of eqv table is #f", Code: `(hashtable-hash-function (make-eqv-hashtable))`, Expected: values.FalseValue},
		{Name: "clear! accepts and ignores k", Code: `(let ((h (make-equal-hashtable))) (hashtable-set! h 'a 1) (hashtable-clear! h 32) (hashtable-size h))`, Expected: values.NewInteger(0)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestHashtableUpdate(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "absent key uses default", Code: `(let ((h (make-equal-hashtable))) (hashtable-update! h 'a (lambda (v) (+ v 1)) 0) (hashtable-ref h 'a #f))`, Expected: values.NewInteger(1)},
		{Name: "present key ignores default", Code: `(let ((h (make-equal-hashtable))) (hashtable-set! h 'a 10) (hashtable-update! h 'a (lambda (v) (+ v 1)) 0) (hashtable-ref h 'a #f))`, Expected: values.NewInteger(11)},
		{Name: "repeated update accumulates", Code: `(let ((h (make-equal-hashtable))) (hashtable-update! h 'a (lambda (v) (+ v 1)) 0) (hashtable-update! h 'a (lambda (v) (+ v 1)) 0) (hashtable-ref h 'a #f))`, Expected: values.NewInteger(2)},
		{Name: "container key", Code: `(let ((h (make-equal-hashtable))) (hashtable-update! h (list 1 2) (lambda (v) (cons 'x v)) '()) (car (hashtable-ref h (list 1 2) #f)))`, Expected: values.NewSymbol("x")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestHashtableUpdateCapturesContinuation is why this is Scheme and not Go. proc
// runs in a Scheme frame, so a continuation captured inside it is capturable
// rather than truncated at a sub-context boundary — the exact failure that moved
// map, for-each, member, and assoc out of Go.
func TestHashtableUpdateCapturesContinuation(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((h (make-equal-hashtable))
		      (k #f))
		  (hashtable-update! h 'a
		    (lambda (v) (+ v (call/cc (lambda (c) (set! k c) 1))))
		    0)
		  (if (procedure? k) 'captured 'truncated))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("captured"))
}

func TestMakeHashtableR6RSArity(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "equal-hash + equal? selects the equal kind", Code: `(let ((h (make-hashtable equal-hash equal?))) (hashtable-set! h (list 1) 'a) (hashtable-ref h (list 1) #f))`, Expected: values.NewSymbol("a")},
		{Name: "optional k accepted", Code: `(hashtable-size (make-hashtable equal-hash equal? 64))`, Expected: values.NewInteger(0)},
		{Name: "keys returns a vector", Code: `(let ((h (make-equal-hashtable))) (hashtable-set! h 'a 1) (vector? (hashtable-keys h)))`, Expected: values.TrueValue},
		{Name: "keys vector has the right length", Code: `(let ((h (make-equal-hashtable))) (hashtable-set! h 'a 1) (hashtable-set! h 'b 2) (vector-length (hashtable-keys h)))`, Expected: values.NewInteger(2)},
		// The two directions of Q6 must agree: what make-hashtable RECOGNIZES is
		// what hashtable-hash-function HANDS BACK.
		{Name: "recognition and accessor agree", Code: `(let ((h (make-hashtable equal-hash equal?))) (and (eq? (hashtable-hash-function h) equal-hash) (eq? (hashtable-equivalence-function h) equal?)))`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMakeHashtableR6RSArityErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "nullary is gone", Code: `(make-hashtable)`},
		{Name: "one argument", Code: `(make-hashtable equal-hash)`},
		{Name: "unsupported hash/equiv pair", Code: `(make-hashtable symbol-hash eq?)`},
		{Name: "user procedures are not supported", Code: `(make-hashtable (lambda (k) 0) (lambda (a b) #t))`},
		// Pointer recognition, not NAME recognition: a user binding that SPELLS
		// equal-hash is a different object and must be refused. Under a .Name()
		// compare a same-named procedure would have been accepted and silently
		// discarded — failing OPEN. These two cases pin that it fails closed.
		{Name: "a rebound equal-hash is refused", Code: `(let ((equal-hash (lambda (k) 0))) (make-hashtable equal-hash equal?))`},
		{Name: "a rebound equal? is refused", Code: `(let ((equal? (lambda (a b) #t))) (make-hashtable equal-hash equal?))`},
		{Name: "hashtable-ref default is required", Code: `(hashtable-ref (make-equal-hashtable) 'a)`},
		{Name: "hashtable-values is gone", Code: `(hashtable-values (make-equal-hashtable))`},
		// R6RS caps make-hashtable at three arguments. ParamCount 3 + IsVariadic
		// means ">= 2", so a fourth used to be swallowed silently — in a table
		// named for arity refusal that tested only the under-arity side.
		{Name: "over-arity is refused", Code: `(make-hashtable equal-hash equal? 1 2)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestMakeHashtableUnsupportedPairSentinel pins that the refusal is matchable,
// not just an error: an embedder gating on the reason needs errors.Is.
func TestMakeHashtableUnsupportedPairSentinel(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, `(make-hashtable symbol-hash eq?)`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrUnsupportedHashtableKind), qt.IsTrue,
		qt.Commentf("must match the sentinel with errors.Is, got %v", err))
}
