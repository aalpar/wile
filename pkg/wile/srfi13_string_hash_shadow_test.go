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
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestStringHashSRFI13StillShadows is the collision guard for the R6RS
// string-hash added to the sealed base.
//
// SRFI-13 exports a BOUNDED string-hash taking an optional bound and range
// (pkg/stdlib/lib/srfi/13/comparison.scm); R6RS's is unary and unbounded.
// Registering the R6RS form in the sealed base must not stop (import (srfi 13))
// from rebinding the name, and must not stop the SRFI-13 library from loading at
// all.
//
// It lives in pkg/wile rather than beside the other string-hash tests in
// pkg/registry/core because that package's test harness has no library registry,
// so (import (srfi 13)) cannot resolve there — on master as well as here.
func TestStringHashSRFI13StillShadows(t *testing.T) {
	c := qt.New(t)
	eng := newSRFITestEngine(t)

	cases := []struct {
		name string
		expr string
		want string
	}{
		{"srfi-13 bounded form still binds", `(begin (import (srfi 13)) (< (string-hash "abcdefghij" 16) 16))`, `#t`},
		{"srfi-13 one-arg form still bounded", `(begin (import (srfi 13)) (< (string-hash "abc") 4194304))`, `#t`},
		{"base form unbounded without the import", `(exact-integer? (string-hash "abc"))`, `#t`},
		// The R6RS names the base ships alongside it, reachable without any import.
		{"base equal-hash without an import", `(= (equal-hash '(1 2)) (equal-hash (list 1 2)))`, `#t`},
		{"base symbol-hash without an import", `(= (symbol-hash 'foo) (symbol-hash 'foo))`, `#t`},
		{"base string-ci-hash without an import", `(= (string-ci-hash "ABC") (string-ci-hash "abc"))`, `#t`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			got := evalSRFI(c, eng, tc.expr)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}
