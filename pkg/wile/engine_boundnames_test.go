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
	"slices"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// TestEngine_BoundNames verifies BoundNames returns sorted, deduplicated names
// spanning all phases plus the sealed base (R21). It is the stable, typed
// alternative to walking Environment().Namespace() phase frames that the REPL
// completer now consumes.
func TestEngine_BoundNames(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)

	names := eng.BoundNames()
	qt.Assert(t, len(names) > 0, qt.IsTrue)
	qt.Assert(t, slices.IsSorted(names), qt.IsTrue,
		qt.Commentf("BoundNames must be sorted"))

	// Sealed-base names must appear: car is a Go primitive, caar a Scheme
	// bootstrap procedure. A runtime-only walk would drop both.
	qt.Assert(t, slices.Contains(names, "car"), qt.IsTrue)
	qt.Assert(t, slices.Contains(names, "caar"), qt.IsTrue)

	// A user define becomes visible.
	_, err = eng.EvalMultiple(ctx, "(define my-new-binding 1)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, slices.Contains(eng.BoundNames(), "my-new-binding"), qt.IsTrue)

	// Deduplicated.
	seen := map[string]bool{}
	for _, n := range names {
		qt.Assert(t, seen[n], qt.IsFalse, qt.Commentf("duplicate name %q", n))
		seen[n] = true
	}
}
