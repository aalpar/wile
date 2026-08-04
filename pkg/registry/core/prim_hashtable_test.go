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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func TestHashtable_MakeAndPredicate(t *testing.T) {
	c := qt.New(t)
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "make-hashtable returns hashtable", Code: `(hashtable? (make-hashtable))`, Expected: values.TrueValue},
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
		{Name: "set and ref round-trip", Code: `(let ((ht (make-hashtable)))
			   (hashtable-set! ht 'key 42)
			   (hashtable-ref ht 'key))`, Expected: values.NewInteger(42)},
		{Name: "ref with default on missing key", Code: `(let ((ht (make-hashtable)))
			   (hashtable-ref ht 'missing 99))`, Expected: values.NewInteger(99)},
		{Name: "integer key", Code: `(let ((ht (make-hashtable)))
			   (hashtable-set! ht 1 "one")
			   (hashtable-ref ht 1))`, Expected: values.NewString("one")},
		{Name: "string key", Code: `(let ((ht (make-hashtable)))
			   (hashtable-set! ht "key" 100)
			   (hashtable-ref ht "key"))`, Expected: values.NewInteger(100)},
		{Name: "boolean key", Code: `(let ((ht (make-hashtable)))
			   (hashtable-set! ht #t "yes")
			   (hashtable-ref ht #t))`, Expected: values.NewString("yes")},
		{Name: "overwrite existing key", Code: `(let ((ht (make-hashtable)))
			   (hashtable-set! ht 'key 1)
			   (hashtable-set! ht 'key 2)
			   (hashtable-ref ht 'key))`, Expected: values.NewInteger(2)},
	}
	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestHashtable_RefErrorOnMissing(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "ref errors on missing key without default", Code: `(let ((ht (make-hashtable)))
			   (hashtable-ref ht 'missing))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

func TestHashtable_Delete(t *testing.T) {
	c := qt.New(t)
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "delete removes key", Code: `(let ((ht (make-hashtable)))
			   (hashtable-set! ht 'key 42)
			   (hashtable-delete! ht 'key)
			   (hashtable-size ht))`, Expected: values.NewInteger(0)},
		{Name: "delete non-existent key is no-op", Code: `(let ((ht (make-hashtable)))
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
		{Name: "empty hashtable size", Code: `(hashtable-size (make-hashtable))`, Expected: values.NewInteger(0)},
		{Name: "size after inserts", Code: `(let ((ht (make-hashtable)))
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

	// Keys returns a list of keys
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((ht (make-hashtable)))
		  (hashtable-set! ht 'only 1)
		  (car (hashtable-keys ht)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewSymbol("only"))

	// Values returns a list of values
	result, err = testhelpers.RunSchemeCode(t, `
		(let ((ht (make-hashtable)))
		  (hashtable-set! ht 'only 99)
		  (car (hashtable-values ht)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(99))

	// Empty hashtable returns empty lists
	result, err = testhelpers.RunSchemeCode(t, `(null? (hashtable-keys (make-hashtable)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.TrueValue)

	result, err = testhelpers.RunSchemeCode(t, `(null? (hashtable-values (make-hashtable)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.TrueValue)
}

func TestHashtable_Copy(t *testing.T) {
	c := qt.New(t)

	// Copy is independent
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((ht (make-hashtable)))
		  (hashtable-set! ht 'a 1)
		  (let ((cp (hashtable-copy ht)))
		    (hashtable-set! ht 'b 2)
		    (hashtable-size cp)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(1))

	// Copy preserves content
	result, err = testhelpers.RunSchemeCode(t, `
		(let ((ht (make-hashtable)))
		  (hashtable-set! ht 'a 1)
		  (let ((cp (hashtable-copy ht)))
		    (hashtable-ref cp 'a)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(1))
}

func TestHashtable_Clear(t *testing.T) {
	c := qt.New(t)
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((ht (make-hashtable)))
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
		(let ((a (make-hashtable))
		      (b (make-hashtable)))
		  (hashtable-set! a 'x 1)
		  (hashtable-set! b 'x 1)
		  (equal? a b))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.TrueValue)

	// equal? on different content
	result, err = testhelpers.RunSchemeCode(t, `
		(let ((a (make-hashtable))
		      (b (make-hashtable)))
		  (hashtable-set! a 'x 1)
		  (hashtable-set! b 'x 2)
		  (equal? a b))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.FalseValue)
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
		{Name: "pair key stores and reads back", Code: `(let ((ht (make-hashtable))) (hashtable-set! ht '(1 2) "val") (hashtable-ref ht '(1 2) #f))`, Expected: values.NewString("val")},
		{Name: "a distinct but equal? pair is the same key", Code: `(let ((ht (make-hashtable))) (hashtable-set! ht (list 1 2) 'a) (hashtable-set! ht (list 1 2) 'b) (hashtable-size ht))`, Expected: values.NewInteger(1)},
		{Name: "vector key round-trips", Code: `(let ((ht (make-hashtable))) (hashtable-set! ht (vector 1 2) 'v) (hashtable-ref ht (vector 1 2) #f))`, Expected: values.NewSymbol("v")},
		{Name: "absent container key returns the default", Code: `(hashtable-ref (make-hashtable) '(1 2) 'missing)`, Expected: values.NewSymbol("missing")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
