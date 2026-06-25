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
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"

	qt "github.com/frankban/quicktest"
)

// compositionEngine builds an engine that can import the sealed stdlib libraries.
func compositionEngine(t *testing.T) *Engine {
	t.Helper()
	eng, err := NewEngine(context.Background(),
		WithProfile(KitchenSink),
		WithSourceFS(stdlib.FS),
		WithLibraryPaths())
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// TestImportSetCompositionOrder pins R7RS §5.6: nested import-set modifiers must be
// applied inside-out in written order, and same-kind nesting must compose, not
// overwrite. Each case here was a wrong answer under the flat-collapse representation
// (libraries-plan Task 5A / compiler-plan 7D). Covers the four 7D compositions plus the
// two same-kind-overwrite cases the compiler plan cross-references.
func TestImportSetCompositionOrder(t *testing.T) {
	testCases := []struct {
		name    string
		program string
		want    string
		wantErr bool
	}{
		{
			// (only (prefix LIB p:) p:car): prefix first ⇒ {p:car, …}; only selects p:car.
			name:    "only-of-prefix",
			program: `(import (only (prefix (scheme base) p:) p:car)) (p:car '(1 2 3))`,
			want:    "1",
		},
		{
			// (rename (prefix LIB p:) (p:car kar)): prefix first ⇒ p:car exists; rename to kar.
			name:    "rename-of-prefix",
			program: `(import (rename (prefix (scheme base) p:) (p:car kar))) (kar '(1 2 3))`,
			want:    "1",
		},
		{
			// (prefix (prefix LIB a-) b-): both prefixes compose ⇒ b-a-car, not b-car.
			name:    "prefix-of-prefix",
			program: `(import (prefix (prefix (scheme base) a-) b-)) (b-a-car '(1 2 3))`,
			want:    "1",
		},
		{
			// The composed prefix means the single-prefix name no longer exists.
			name:    "prefix-of-prefix-single-prefix-gone",
			program: `(import (prefix (prefix (scheme base) a-) b-)) (b-car '(1 2 3))`,
			wantErr: true,
		},
		{
			// (only (only LIB car) cdr): inner restricts to {car}; outer cannot select cdr.
			name:    "only-of-only-outside-inner-errors",
			program: `(import (only (only (scheme base) car) cdr)) (cdr '(1 2 3))`,
			wantErr: true,
		},
		{
			// (only (except (only LIB car cdr) cdr) car): only{car,cdr} → except cdr → only car.
			name:    "only-except-only-chain",
			program: `(import (only (except (only (scheme base) car cdr) cdr) car)) (car '(1 2 3))`,
			want:    "1",
		},
		{
			// (rename LIB (car kar) (cdr kar)): two exports collapse to one name ⇒ error.
			// (Empty-only's "import nothing" is verified at the binding-map level in
			// pkg/machine/compilation; KitchenSink binds core primitives ambiently, so it
			// is not observable through an engine-level reference to car.)
			name:    "rename-collision-errors",
			program: `(import (rename (scheme base) (car kar) (cdr kar)))`,
			wantErr: true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			eng := compositionEngine(t)
			result, err := eng.EvalMultiple(ctx, tc.program)
			if tc.wantErr {
				c.Assert(err, qt.IsNotNil)
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}
