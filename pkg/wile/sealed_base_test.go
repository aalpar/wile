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
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	valuestest "github.com/aalpar/wile/values/valuestest"
)

// TestSealedBase_G2_StdlibProcedureDocs is the G2 regression. Post-carve, Scheme-defined
// bootstrap procedures (e.g. caar) live in the sealed base, not the mutable runtime
// child. Two indexing paths must read the sealed base:
//   - procedure-documentation reads the procedure value's own doc (the closure landed in
//     the sealed base WITH its docstring).
//   - registerSchemeDocstrings walks the sealed base to register doc-only entries for
//     ,apropos/,doc/SearchDoc; reading the empty mutable child would drop them all.
func TestSealedBase_G2_StdlibProcedureDocs(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		qt.Assert(t, eng.Close(), qt.IsNil)
	}()

	// procedure-documentation: the caar closure (in the sealed base) carries its doc.
	result, err := eng.EvalMultiple(ctx, `(procedure-documentation caar)`)
	qt.Assert(t, err, qt.IsNil)
	doc, ok := result.Internal().(*values.String)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, strings.Contains(doc.String(), "Category: pairs"), qt.IsTrue)

	// registerSchemeDocstrings: the registry doc-only index includes caar (read from the
	// sealed base). If it read the mutable child's empty frame, caar would be absent.
	found := false
	for _, dp := range eng.Registry().DocPrimitives() {
		if dp.Name == "caar" {
			found = true
			qt.Assert(t, dp.Category, qt.Equals, "pairs")
			break
		}
	}
	qt.Assert(t, found, qt.IsTrue)
}

// TestSealedBase_PrimitiveGlobalIndexPinsSealedBase verifies compiled references resolve
// through the frame-pinned GlobalIndex into the sealed base, for primitives, stdlib
// procedures, and user defines (in the mutable child reaching the sealed base).
func TestSealedBase_PrimitiveGlobalIndexPinsSealedBase(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{"primitive call resolves post-carve", `(+ 1 2 3)`, values.NewInteger(6)},
		{"stdlib proc resolves post-carve", `(caar '((1 2) 3))`, values.NewInteger(1)},
		{"user define then call", `(define (sq x) (* x x)) (sq 9)`, values.NewInteger(81)},
	}
	ctx := context.Background()
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
			qt.Assert(t, err, qt.IsNil)
			defer func() {
				qt.Assert(t, eng.Close(), qt.IsNil)
			}()
			result, err := eng.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.Internal(), valuestest.SchemeEquals, tc.expected)
		})
	}
}
