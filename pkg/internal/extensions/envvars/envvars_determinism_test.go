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

package envvars_test

import (
	"fmt"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// keyOrder renders just the key sequence of (get-environment-variables), so a
// reordering shows up as a different string.
const keyOrder = `
	(let loop ((vars (get-environment-variables)) (acc ""))
	  (if (null? vars)
	      acc
	      (loop (cdr vars) (string-append acc (car (car vars)) ";"))))
`

// TestGetEnvironmentVariables_VirtualMapIsStable pins that the virtual-env
// branch returns one order. It ranged a bare Go map, whose iteration order is
// randomized PER CALL, so eight keys gave four distinct orders in six calls
// inside a single engine — while the os.Environ sibling walks a slice and has
// always been stable. Unlike the hashtable-rendering defect this needs no fork:
// the disorder is per-call, not per-process.
//
// This is house-rule compliance (REVIEW.md "Nondeterminism in Returned Lists"),
// not conformance: R7RS leaves the alist's order unspecified, and the repo's own
// vendored suite comments the order-sensitive assertion out.
func TestGetEnvironmentVariables_VirtualMapIsStable(t *testing.T) {
	c := qt.New(t)
	env := map[string]string{}
	for i := range 20 {
		env[fmt.Sprintf("VK%02d", i)] = fmt.Sprintf("v%d", i)
	}
	engine := newSandboxedEngine(t, env)

	first := runScheme(t, engine, keyOrder).Internal().SchemeString()
	for i := range 12 {
		got := runScheme(t, engine, keyOrder).Internal().SchemeString()
		c.Assert(got, qt.Equals, first, qt.Commentf("call %d disagreed with call 0", i+1))
	}

	// Sorted ascending, matching the os.Environ branch's shape (that branch
	// preserves os.Environ's own order; this one has no source order to
	// preserve, so it picks the only content-derived total order there is).
	want := &strings.Builder{}
	for i := range 20 {
		fmt.Fprintf(want, "VK%02d;", i)
	}
	c.Assert(first, qt.Equals, `"`+want.String()+`"`)
}
