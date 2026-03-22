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

package integration_test

// Tests that call/cc works correctly inside higher-order function callbacks.
//
// These functions were migrated from Go to Scheme (bootstrap_procedures.scm)
// because Go iteration loops cannot produce capturable Scheme continuation
// frames. The Go for-loop + sub.Run() pattern silently breaks call/cc:
// the captured continuation cannot re-enter the Go loop since its stack
// frame is gone.
//
// Each test captures a continuation inside a callback, then re-invokes it
// to verify the iteration loop resumes correctly from that point.

import (
	"context"
	"testing"

	"github.com/aalpar/wile"

	qt "github.com/frankban/quicktest"
)

func TestCallCC_InsideCallback(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name: "vector-map",
			// Capture continuation at element 2, then re-invoke with 99.
			// The loop resumes from i=1, setting result[1]=99.
			code: `
				(let ((k #f))
				  (let ((v (vector-map (lambda (x)
				                         (if (= x 2)
				                             (call/cc (lambda (c) (set! k c) x))
				                             x))
				                       '#(1 2 3))))
				    (if k
				        (let ((saved-k k))
				          (set! k #f)
				          (saved-k 99))
				        v)))`,
			expected: "#(1 99 3)",
		},
		{
			name: "vector-for-each",
			// Capture continuation at element 2, then re-invoke.
			// First pass: count reaches 3 (elements 1,2,3).
			// Re-invocation resumes after (set! count ...) for x=2,
			// runs remaining element (x=3) → count=4.
			code: `
				(let ((k #f)
				      (count 0))
				  (vector-for-each (lambda (x)
				                     (set! count (+ count 1))
				                     (when (and (= x 2) (not k))
				                       (call/cc (lambda (c) (set! k c)))))
				                   '#(1 2 3))
				  (if k
				      (let ((saved-k k))
				        (set! k #f)
				        (saved-k 'ignored))
				      count))`,
			expected: "4",
		},
		{
			name: "string-map",
			// Capture continuation at char #\b, then re-invoke with #\X.
			// The loop resumes from i=1, setting result[1]=#\X.
			code: `
				(let ((k #f))
				  (let ((s (string-map (lambda (c)
				                         (if (char=? c #\b)
				                             (call/cc (lambda (cont) (set! k cont) c))
				                             c))
				                       "abc")))
				    (if k
				        (let ((saved-k k))
				          (set! k #f)
				          (saved-k #\X))
				        s)))`,
			expected: `"aXc"`,
		},
		{
			name: "string-for-each",
			// Same pattern as vector-for-each: capture at #\b, re-invoke.
			// First pass: count reaches 3. Re-invocation runs from
			// after call/cc for #\b onward (only #\c remains) → count=4.
			code: `
				(let ((k #f)
				      (count 0))
				  (string-for-each (lambda (c)
				                     (set! count (+ count 1))
				                     (when (and (char=? c #\b) (not k))
				                       (call/cc (lambda (cont) (set! k cont)))))
				                   "abc")
				  (if k
				      (let ((saved-k k))
				        (set! k #f)
				        (saved-k 'ignored))
				      count))`,
			expected: "4",
		},
		{
			name: "member with custom comparator",
			// Search for 99 (not in list) with custom comparator.
			// Capture continuation at element 2, returning #f (no match).
			// Re-invoke with #t → comparator returns #t → member finds (2 3).
			code: `
				(let ((k #f))
				  (let ((result (member 99 '(1 2 3)
				                        (lambda (a b)
				                          (if (and (= b 2) (not k))
				                              (call/cc (lambda (c) (set! k c) #f))
				                              (= a b))))))
				    (if k
				        (let ((saved-k k))
				          (set! k #f)
				          (saved-k #t))
				        result)))`,
			expected: "(2 3)",
		},
		{
			name: "assoc with custom comparator",
			// Search for 99 (not in alist) with custom comparator.
			// Capture continuation at key 2, returning #f (no match).
			// Re-invoke with #t → comparator returns #t → assoc finds (2 b).
			code: `
				(let ((k #f))
				  (let ((result (assoc 99 '((1 a) (2 b) (3 c))
				                       (lambda (a b)
				                         (if (and (= b 2) (not k))
				                             (call/cc (lambda (c) (set! k c) #f))
				                             (= a b))))))
				    (if k
				        (let ((saved-k k))
				          (set! k #f)
				          (saved-k #t))
				        result)))`,
			expected: "(2 b)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithSafeExtensions())
			c.Assert(err, qt.IsNil)

			result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), tc.code))
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}
