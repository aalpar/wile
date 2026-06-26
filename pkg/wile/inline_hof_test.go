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
		"map", "for-each", "vector-map", "vector-for-each", "string-map", "string-for-each",
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

	// Non-curated: non-HOFs (car/cons) and a HOF outside the curated set (apply is
	// a HOF but not curated) all read -1 — the curation is selective, not "every HOF."
	notInlineHOFs := []string{"car", "cons", "apply"}
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

// TestInlineHOFStamp_FoldViaImport pins that the import-gated curated HOF (fold,
// srfi/1) acquires the InlineHOF capability when imported. fold is not bootstrap-
// resident, so the post-bootstrap sweep cannot reach it; the import path stamps
// the freshly-created target binding by export name instead. Orthogonality holds:
// fold applies its callback (kons) and stays NOT capture-safe.
func TestInlineHOFStamp_FoldViaImport(t *testing.T) {
	eng := captureSafetyEngine(t)
	ctx := context.Background()

	// Before import, fold is unbound (or at least not a curated inline HOF).
	preEnv := eng.Environment()
	preBinding := preEnv.GetBinding(values.NewSymbol("fold"), nil)
	if preBinding != nil {
		qt.Assert(t, preBinding.InlineHOFParam(), qt.Equals, -1,
			qt.Commentf("fold must not be a curated inline HOF before (import (srfi 1))"))
	}

	_, err := eng.EvalMultiple(ctx, "(import (srfi 1))")
	qt.Assert(t, err, qt.IsNil)

	env := eng.Environment()
	b := env.GetBinding(values.NewSymbol("fold"), nil)
	qt.Assert(t, b, qt.IsNotNil, qt.Commentf("fold must be bound after (import (srfi 1))"))
	qt.Assert(t, b.InlineHOFParam(), qt.Equals, 0,
		qt.Commentf("imported fold is a curated tail HOF; its callback (kons) is parameter 0"))
	qt.Assert(t, b.IsCaptureSafe(), qt.IsFalse,
		qt.Commentf("fold applies its callback and MUST stay NOT capture-safe"))
}

// TestInlineHOFStamp_RenamedImport pins that the capability follows the library
// export, not the local alias: an import-rename of fold still stamps the renamed
// binding, because the stamp keys on the external name.
func TestInlineHOFStamp_RenamedImport(t *testing.T) {
	eng := captureSafetyEngine(t)
	ctx := context.Background()

	_, err := eng.EvalMultiple(ctx, "(import (rename (srfi 1) (fold my-fold)))")
	qt.Assert(t, err, qt.IsNil)

	env := eng.Environment()
	b := env.GetBinding(values.NewSymbol("my-fold"), nil)
	qt.Assert(t, b, qt.IsNotNil, qt.Commentf("my-fold must be bound after the renamed import"))
	qt.Assert(t, b.InlineHOFParam(), qt.Equals, 0,
		qt.Commentf("the renamed fold still carries the capability (keyed on the export name)"))
}

// TestInlineHOFStamp_UserDefineNotStamped pins the soundness boundary: a user
// procedure that merely shares a curated HOF's name but is NOT a library export
// (a plain top-level define) is never stamped. The import path is the only library
// seam; a user define lands in the mutable runtime and reads -1.
func TestInlineHOFStamp_UserDefineNotStamped(t *testing.T) {
	eng := captureSafetyEngine(t)
	ctx := context.Background()

	// Define a procedure named "fold" without importing srfi/1.
	_, err := eng.EvalMultiple(ctx, "(define (fold a b) a)")
	qt.Assert(t, err, qt.IsNil)

	env := eng.Environment()
	b := env.GetBinding(values.NewSymbol("fold"), nil)
	qt.Assert(t, b, qt.IsNotNil)
	qt.Assert(t, b.InlineHOFParam(), qt.Equals, -1,
		qt.Commentf("a user define is not a library export and must NOT be stamped an inline HOF"))
}

// TestInlineHOFStamp_Srfi13StringMapNotStamped pins the soundness fix for the
// over-broad import-path stamp: the import path stamps ONLY import-gated curated
// HOFs (fold), never a re-export of a SEALED-BASE name. SRFI-13 exports its OWN
// string-map (single-string + optional range, NOT the R7RS variadic form);
// importing it must not stamp that binding, or (string-map f s) would inline the
// R7RS template for SRFI-13's different procedure. The genuine R7RS string-map is
// stamped at its sealed-base home (TestInlineHOFStamp); only that binding inlines.
func TestInlineHOFStamp_Srfi13StringMapNotStamped(t *testing.T) {
	eng := captureSafetyEngine(t)
	ctx := context.Background()

	_, err := eng.EvalMultiple(ctx, "(import (srfi 13))")
	qt.Assert(t, err, qt.IsNil)

	env := eng.Environment()
	b := env.GetBinding(values.NewSymbol("string-map"), nil)
	qt.Assert(t, b, qt.IsNotNil, qt.Commentf("string-map is bound (SRFI-13 shadows R7RS)"))
	qt.Assert(t, b.InlineHOFParam(), qt.Equals, -1,
		qt.Commentf("SRFI-13's string-map is a sealed-base re-export, not an import-gated HOF; "+
			"the import path must not stamp it (it differs from R7RS string-map)"))
}
