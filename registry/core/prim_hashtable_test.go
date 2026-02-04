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

	"github.com/aalpar/wile/values"
)

func TestHashtable_MakeAndPredicate(t *testing.T) {
	c := qt.New(t)
	tcs := []schemeCodeTestCase{
		{"make-hashtable returns hashtable", `(hashtable? (make-hashtable))`, values.TrueValue},
		{"integer is not hashtable", `(hashtable? 42)`, values.FalseValue},
		{"string is not hashtable", `(hashtable? "hello")`, values.FalseValue},
		{"pair is not hashtable", `(hashtable? '(1 2))`, values.FalseValue},
	}
	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestHashtable_SetAndRef(t *testing.T) {
	c := qt.New(t)
	tcs := []schemeCodeTestCase{
		{
			"set and ref round-trip",
			`(let ((ht (make-hashtable)))
			   (hashtable-set! ht 'key 42)
			   (hashtable-ref ht 'key))`,
			values.NewInteger(42),
		},
		{
			"ref with default on missing key",
			`(let ((ht (make-hashtable)))
			   (hashtable-ref ht 'missing 99))`,
			values.NewInteger(99),
		},
		{
			"integer key",
			`(let ((ht (make-hashtable)))
			   (hashtable-set! ht 1 "one")
			   (hashtable-ref ht 1))`,
			values.NewString("one"),
		},
		{
			"string key",
			`(let ((ht (make-hashtable)))
			   (hashtable-set! ht "key" 100)
			   (hashtable-ref ht "key"))`,
			values.NewInteger(100),
		},
		{
			"boolean key",
			`(let ((ht (make-hashtable)))
			   (hashtable-set! ht #t "yes")
			   (hashtable-ref ht #t))`,
			values.NewString("yes"),
		},
		{
			"overwrite existing key",
			`(let ((ht (make-hashtable)))
			   (hashtable-set! ht 'key 1)
			   (hashtable-set! ht 'key 2)
			   (hashtable-ref ht 'key))`,
			values.NewInteger(2),
		},
	}
	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestHashtable_RefErrorOnMissing(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			"ref errors on missing key without default",
			`(let ((ht (make-hashtable)))
			   (hashtable-ref ht 'missing))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestHashtable_Delete(t *testing.T) {
	c := qt.New(t)
	tcs := []schemeCodeTestCase{
		{
			"delete removes key",
			`(let ((ht (make-hashtable)))
			   (hashtable-set! ht 'key 42)
			   (hashtable-delete! ht 'key)
			   (hashtable-size ht))`,
			values.NewInteger(0),
		},
		{
			"delete non-existent key is no-op",
			`(let ((ht (make-hashtable)))
			   (hashtable-delete! ht 'missing)
			   (hashtable-size ht))`,
			values.NewInteger(0),
		},
	}
	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestHashtable_Size(t *testing.T) {
	c := qt.New(t)
	tcs := []schemeCodeTestCase{
		{
			"empty hashtable size",
			`(hashtable-size (make-hashtable))`,
			values.NewInteger(0),
		},
		{
			"size after inserts",
			`(let ((ht (make-hashtable)))
			   (hashtable-set! ht 'a 1)
			   (hashtable-set! ht 'b 2)
			   (hashtable-set! ht 'c 3)
			   (hashtable-size ht))`,
			values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestHashtable_KeysAndValues(t *testing.T) {
	c := qt.New(t)

	// Keys returns a list of keys
	result, err := runSchemeCode(t, `
		(let ((ht (make-hashtable)))
		  (hashtable-set! ht 'only 1)
		  (car (hashtable-keys ht)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, values.SchemeEquals, values.NewSymbol("only"))

	// Values returns a list of values
	result, err = runSchemeCode(t, `
		(let ((ht (make-hashtable)))
		  (hashtable-set! ht 'only 99)
		  (car (hashtable-values ht)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, values.SchemeEquals, values.NewInteger(99))

	// Empty hashtable returns empty lists
	result, err = runSchemeCode(t, `(null? (hashtable-keys (make-hashtable)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, values.SchemeEquals, values.TrueValue)

	result, err = runSchemeCode(t, `(null? (hashtable-values (make-hashtable)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, values.SchemeEquals, values.TrueValue)
}

func TestHashtable_Copy(t *testing.T) {
	c := qt.New(t)

	// Copy is independent
	result, err := runSchemeCode(t, `
		(let ((ht (make-hashtable)))
		  (hashtable-set! ht 'a 1)
		  (let ((cp (hashtable-copy ht)))
		    (hashtable-set! ht 'b 2)
		    (hashtable-size cp)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, values.SchemeEquals, values.NewInteger(1))

	// Copy preserves content
	result, err = runSchemeCode(t, `
		(let ((ht (make-hashtable)))
		  (hashtable-set! ht 'a 1)
		  (let ((cp (hashtable-copy ht)))
		    (hashtable-ref cp 'a)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, values.SchemeEquals, values.NewInteger(1))
}

func TestHashtable_Clear(t *testing.T) {
	c := qt.New(t)
	result, err := runSchemeCode(t, `
		(let ((ht (make-hashtable)))
		  (hashtable-set! ht 'a 1)
		  (hashtable-set! ht 'b 2)
		  (hashtable-clear! ht)
		  (hashtable-size ht))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, values.SchemeEquals, values.NewInteger(0))
}

func TestHashtable_EqualQ(t *testing.T) {
	c := qt.New(t)

	// equal? on identical content
	result, err := runSchemeCode(t, `
		(let ((a (make-hashtable))
		      (b (make-hashtable)))
		  (hashtable-set! a 'x 1)
		  (hashtable-set! b 'x 1)
		  (equal? a b))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, values.SchemeEquals, values.TrueValue)

	// equal? on different content
	result, err = runSchemeCode(t, `
		(let ((a (make-hashtable))
		      (b (make-hashtable)))
		  (hashtable-set! a 'x 1)
		  (hashtable-set! b 'x 2)
		  (equal? a b))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, values.SchemeEquals, values.FalseValue)
}

func TestHashtable_TypeErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"hashtable-ref on non-hashtable", `(hashtable-ref 42 'key)`},
		{"hashtable-set! on non-hashtable", `(hashtable-set! 42 'key 1)`},
		{"hashtable-delete! on non-hashtable", `(hashtable-delete! 42 'key)`},
		{"hashtable-keys on non-hashtable", `(hashtable-keys 42)`},
		{"hashtable-values on non-hashtable", `(hashtable-values 42)`},
		{"hashtable-size on non-hashtable", `(hashtable-size 42)`},
		{"hashtable-copy on non-hashtable", `(hashtable-copy 42)`},
		{"hashtable-clear! on non-hashtable", `(hashtable-clear! 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestHashtable_NonComparableKeyError(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			"set with non-comparable key",
			`(let ((ht (make-hashtable)))
			   (hashtable-set! ht '(1 2) "val"))`,
		},
		{
			"ref with non-comparable key",
			`(let ((ht (make-hashtable)))
			   (hashtable-ref ht '(1 2)))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}
