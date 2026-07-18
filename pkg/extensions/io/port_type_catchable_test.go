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

package io_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestCurrentPortTypeErrorIsCatchable pins that a wrong-typed current port
// installed via parameterize raises a guard-catchable condition, not an
// uncatchable host-boundary panic. Before the fix, resolveCurrent{Input,Output}Port
// panic and the panic escapes guard as a *SchemeError at the RunResumable boundary.
func TestCurrentPortTypeErrorIsCatchable(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
	}{
		{
			name: "wrong-typed current-output-port is guard-catchable",
			code: `(eq? 'caught
			         (guard (e (#t 'caught))
			           (parameterize ((current-output-port 5)) (display "x"))))`,
		},
		{
			name: "wrong-typed current-input-port is guard-catchable",
			code: `(eq? 'caught
			         (guard (e (#t 'caught))
			           (parameterize ((current-input-port 5)) (read-char))))`,
		},
		{
			name: "wrong-typed current-output-port via write-string is guard-catchable",
			code: `(eq? 'caught
			         (guard (e (#t 'caught))
			           (parameterize ((current-output-port 5)) (write-string "x"))))`,
		},
		{
			name: "existing car type error still guard-catchable (control)",
			code: `(eq? 'caught (guard (e (#t 'caught)) (car 5)))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, values.TrueValue)
		})
	}
}
