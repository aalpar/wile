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

package environment_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	valuestest "github.com/aalpar/wile/values/valuestest"
)

// runSteps evaluates each step as a SEPARATE compilation unit against one shared
// environment, returning the last step's value. Separate units (rather than one
// (begin ...) wrapper) are essential for the redefine-visibility characterization:
// a closure compiled in an earlier unit pins its global references at compile time,
// and a later unit's redefine is what exposes the sealed-base shadow semantics. This
// mirrors how EvalMultiple and real REPL input compile top-level forms.
func runSteps(t *testing.T, steps ...string) values.Value {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrame(context.Background())
	qt.Assert(t, err, qt.IsNil)
	var result values.Value
	for _, step := range steps {
		result, err = testhelpers.RunSchemeCodeWithEnv(t, env, step)
		qt.Assert(t, err, qt.IsNil)
	}
	return result
}

// TestCharacterization_UserDefineShadowsPrimitive pins the contracts the carve must
// preserve or deliberately change.
//
// Cases 1-2 are resolution contracts that MUST stay green through Phases 1-3.
// Case 3 is the redefine-VISIBILITY contract that WILL change in Phase 1 (Chez
// sealed-base semantics): today an already-compiled closure over a sealed name
// sees a later redefine; post-carve it sees the sealed value. The expected value
// below is TODAY'S observed value; Phase 1 Step 4 flips it.
func TestCharacterization_UserDefineShadowsPrimitive(t *testing.T) {
	tcs := []struct {
		name     string
		steps    []string
		expected values.Value
	}{
		{
			name:     "shadow car with user define",
			steps:    []string{`(define car (lambda (x) 42))`, `(car '(1 2))`},
			expected: values.NewInteger(42),
		},
		{
			name:     "primitive intact through define-then-shadow",
			steps:    []string{`(define (use-car p) (car p))`, `(define car (lambda (x) 99))`, `(use-car '(7 8))`},
			expected: values.NewInteger(99),
		},
		// Redefine-visibility: call the closure FIRST, then shadow, then call again.
		// This is the interleaving that exposes the cached-binding-pointer change.
		{
			name:     "closure observes redefine (PRE-carve behavior)",
			steps:    []string{`(define (use-car p) (car p))`, `(use-car '(1 2))`, `(define car (lambda (x) 99))`, `(use-car '(7 8))`},
			expected: values.NewInteger(99),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := runSteps(t, tc.steps...)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

// TestCharacterization_RuntimeFrameTopology pins the CURRENT topology so the carve
// is a visible, deliberate change: today the runtime frame is the root (parent nil).
//
// Phase 1 Step 4 INVERTS this test (after the carve, Runtime() is the mutable child
// whose parent is the sealed base — IsTopLevel() becomes false).
func TestCharacterization_RuntimeFrameTopology(t *testing.T) {
	ns := environment.NewNamespace()
	rt := ns.Runtime()
	qt.Assert(t, rt.IsTopLevel(), qt.IsTrue) // parent == nil today
}
