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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestInlineHOFStamp pins Phase 1 of callback specialization Strategy A: the
// curated bootstrap tail HOFs carry the inert InlineHOF capability (callback at
// parameter index 0), while non-curated procedures do not.
//
// The capability is ORTHOGONAL to CaptureSafe: for-each stays NOT capture-safe
// (it applies its callback, which may capture); the stamp only says "inlinable
// WHEN the callback is independently proven safe." The stamp is inert in this
// phase — no compiler dispatch consults it yet. (fold is import-gated via srfi/1,
// not bootstrap-resident, so it is stamped on its import path in a later phase and
// is intentionally absent here.)
func TestInlineHOFStamp(t *testing.T) {
	eng := captureSafetyEngine(t)
	env := eng.Environment()

	// Curated tail HOFs resident in the sealed base (bootstrap_procedures.scm).
	// Each takes its callback as the first parameter -> index 0.
	inlineHOFs := []string{
		"for-each", "vector-map", "vector-for-each", "string-map", "string-for-each",
	}
	for _, name := range inlineHOFs {
		t.Run("hof/"+name, func(t *testing.T) {
			c := qt.New(t)
			b := env.GetBinding(values.NewSymbol(name), nil)
			c.Assert(b, qt.IsNotNil,
				qt.Commentf("%q must be bound in KitchenSink", name))
			c.Assert(b.InlineHOFParam(), qt.Equals, 0,
				qt.Commentf("%q is a curated tail HOF; its callback is parameter 0", name))
			// Orthogonality: the inline-HOF stamp must NOT make it capture-safe.
			c.Assert(b.IsCaptureSafe(), qt.IsFalse,
				qt.Commentf("%q applies its callback (may capture) and MUST stay NOT capture-safe", name))
		})
	}

	// Non-curated: a non-HOF (car/cons) and a HOF outside the v1 set (map is
	// non-tail, deferred) all read -1 — the curation is selective, not "every HOF."
	notInlineHOFs := []string{"car", "cons", "map"}
	for _, name := range notInlineHOFs {
		t.Run("non/"+name, func(t *testing.T) {
			c := qt.New(t)
			b := env.GetBinding(values.NewSymbol(name), nil)
			c.Assert(b, qt.IsNotNil, qt.Commentf("%q must be bound", name))
			c.Assert(b.InlineHOFParam(), qt.Equals, -1,
				qt.Commentf("%q is not a curated inline HOF and must read -1", name))
		})
	}
}
