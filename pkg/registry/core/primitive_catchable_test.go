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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// TestPrimitiveConditionsAreCatchable pins the invariant that a bad input
// reachable from Scheme raises a guard-catchable condition, never an uncatchable
// host-boundary panic. Unlike RunSchemeCodeExpectError (which scores a recovered
// panic as a satisfied error), RunSchemeCodeExpectCatchable fails on any escaped
// Go panic and on any condition that guard cannot catch. The current-port rows are
// the regression guard for the io resolve-time panic.
func TestPrimitiveConditionsAreCatchable(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "car of non-pair", Code: `(car 5)`},
		{Name: "vector-ref out of range", Code: `(vector-ref (vector) 0)`},
		{Name: "division by zero", Code: `(/ 1 0)`},
		{Name: "wrong-typed current-output-port via display", Code: `(parameterize ((current-output-port 5)) (display "x"))`},
		{Name: "wrong-typed current-output-port via write-string", Code: `(parameterize ((current-output-port 5)) (write-string "x"))`},
		{Name: "wrong-typed current-input-port via read-char", Code: `(parameterize ((current-input-port 5)) (read-char))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectCatchable(t, tc.Code)
		})
	}
}
