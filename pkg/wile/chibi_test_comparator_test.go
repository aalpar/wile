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

package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// TestChibiTestComparator covers plan item 9B: (chibi test)'s test macro must
// (1) compare inexact numbers nested in lists/vectors approximately, not only
// bare inexact numbers, and (2) never bypass an explicitly-set
// current-test-comparator — including for inexact expected values.
func TestChibiTestComparator(t *testing.T) {
	t.Parallel()

	ctx := context.Background()

	// The handle is the SUBTEST's: a FailNow on the parent's, from the
	// subtest's goroutine, aborts the wrong test.
	newEngine := func(c *qt.C) *wile.Engine {
		eng, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(stdlib.FS),
			wile.WithLibraryPaths("."),
		)
		c.Assert(err, qt.IsNil)
		return eng
	}

	tcs := []struct {
		name string
		code string
		want string
	}{
		// %approx-equal? recurses through lists/vectors for near-equal inexacts.
		{
			name: "approx list near-equal inexact",
			code: `(import (chibi test)) (%approx-equal? (list (+ 1.0 1e-9)) (list 1.0) 1e-5)`,
			want: "#t",
		},
		{
			name: "approx vector near-equal inexact",
			code: `(import (chibi test)) (%approx-equal? (vector (+ 1.0 1e-9)) (vector 1.0) 1e-5)`,
			want: "#t",
		},
		// Exact numbers (even nested) compare precisely: distinct exacts differ.
		{
			name: "approx exact distinct in list",
			code: `(import (chibi test)) (%approx-equal? (list 1000000) (list 1000001) 1e-5)`,
			want: "#f",
		},
		// The test macro routes a list-of-inexact through %approx-equal?: 0 fails.
		{
			name: "test macro list inexact passes",
			code: `(import (chibi test))
                   (test-begin "nested")
                   (test (list (+ 1.0 1e-9)) (list 1.0))
                   ((test-failure-count))`,
			want: "0",
		},
		// An explicit comparator is honored even for inexact expected: a #f
		// comparator makes (test 1.0 1.0) fail (it is not bypassed by approx).
		// The harness's report goes to a string port and is part of the answer:
		// on the process's stdout this deliberate FAIL line reached go test's
		// output unattributed, beside whichever parallel test was running.
		{
			name: "custom comparator not bypassed for inexact",
			code: `(import (chibi test))
                   (define out (open-output-string))
                   (parameterize ((current-output-port out))
                     (test-begin "custom")
                     (parameterize ((current-test-comparator (lambda (e a) #f)))
                       (test 1.0 1.0)))
                   (list ((test-failure-count)) (get-output-string out))`,
			want: `(1 "Testing custom\n  FAIL: expected 1.0 but got 1.0\n")`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			eng := newEngine(c)
			result, err := eng.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}
