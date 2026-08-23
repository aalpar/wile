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
)

// TestQuasiquoteImproperUnquoteTail_NoPanic pins the fix for an improper-list
// panic in the quasiquote walks. An unquote / unquote-splicing in spine position
// followed by an improper (dotted) tail — e.g. `(a unquote x . y) — used to reach
// SyntaxPair.Length(), which panics on an improper list, and surfaced as an
// opaque "compile: panic recovery: internal error: SyntaxPair.Length: improper
// list" instead of a clean compile. Both the validator (dottedUnquoteTail) and
// the compiler (compileQuasiquoteDatum / quasiNeedsRuntime /
// quasiNeedsRuntimeList) walked it; all now use a properness-safe
// IsSyntaxEmptyList check. A malformed template compiles as ordinary quasiquoted
// data, so a clean nil error proves the safe path rather than the crash path.
func TestQuasiquoteImproperUnquoteTail_NoPanic(t *testing.T) {
	cases := []struct {
		name string
		code string
	}{
		{"dotted unquote tail", "`(a unquote x . y)"},
		{"dotted splicing tail", "`(a unquote-splicing . y)"},
		{"bare unquote improper tail", "`(unquote . y)"},
		{"nested template improper tail", "`(a `(b unquote . y))"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng := newTestEngine(t)
			defer func() {
				qt.Assert(t, eng.Close(), qt.IsNil)
			}()

			_, err := eng.EvalMultiple(context.Background(), tc.code)
			qt.Assert(t, err, qt.IsNil, qt.Commentf("code: %s", tc.code))
		})
	}
}
