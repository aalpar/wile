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
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"

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

// TestImportConflictDetection pins Wile's Racket-style strict resolution of aliased
// imports (R7RS §5.6, "it is an error to import the same identifier more than once
// with different bindings"): a same-name import that resolves to a DIFFERENT binding
// is signalled as ErrDuplicateBinding rather than silently last-import-wins. The whole
// stdlib has exactly one such genuine collision — (scheme base) string-map (variadic,
// primitive) vs (srfi 13) string-map (single-string + range). Diamonds (a name two
// libraries re-export from one source) and repeated imports stay legal. The resolution
// paths a program can use to disambiguate (except / prefix, or the R5RS base that
// never had string-map) are pinned too.
func TestImportConflictDetection(t *testing.T) {
	testCases := []struct {
		name    string
		program string
		want    string
		wantErr bool
	}{
		{
			// base and srfi-13 bind string-map to DIFFERENT procedures ⇒ conflict.
			name:    "base-and-srfi13-string-map-conflicts",
			program: `(import (scheme base) (srfi 13))`,
			wantErr: true,
		},
		{
			// srfi-1 re-exports base's make-list/list-copy verbatim ⇒ diamonds, no conflict.
			name:    "base-and-srfi1-diamonds-ok",
			program: `(import (scheme base) (srfi 1)) (make-list 2 'x)`,
			want:    "(x x)",
		},
		{
			// re-importing the same library is idempotent, never a self-conflict.
			name:    "reimport-same-library-ok",
			program: `(import (scheme base) (scheme base)) (car '(1 2 3))`,
			want:    "1",
		},
		{
			// excepting the colliding name from base resolves it: srfi-13's range form wins.
			name:    "except-resolves-conflict",
			program: `(import (except (scheme base) string-map) (srfi 13)) (string=? "ELL" (string-map char-upcase "hello" 1 4))`,
			want:    "#t",
		},
		{
			// prefixing the second library renames the colliding name away ⇒ no conflict.
			name:    "prefix-resolves-conflict",
			program: `(import (scheme base) (prefix (srfi 13) s:)) (string=? "ELL" (s:string-map char-upcase "hello" 1 4))`,
			want:    "#t",
		},
		{
			// R5RS base never had string-map ⇒ (scheme r5rs) + (srfi 13) is conflict-free.
			name:    "r5rs-base-no-conflict",
			program: `(import (scheme r5rs) (srfi 13)) (string=? "ell" (string-map (lambda (c) c) "hello" 1 4))`,
			want:    "#t",
		},
		{
			// define-over-import: a user (define zero?) after (import (scheme base))
			// legally shadows the imported zero? — it is NOT an import conflict. (A
			// source-location origin would falsely flag this; name comparison preserves
			// shadowing. See r7rs-differences.md.)
			name:    "define-over-import-shadows-not-conflict",
			program: `(import (scheme base)) (define (zero? x) (eq? x 'z)) (list (zero? 'z) (zero? 5))`,
			want:    "(#t #f)",
		},
		{
			// Macro re-export diamond: (scheme base) and (scheme r5rs) both re-export the
			// ambient derived-syntax macros (let, cond, …). These are name-less,
			// recompiled-per-manifest closures, so they MUST be recognized as the same
			// binding by name — a value-identity check would falsely flag this common
			// combination as a conflict (regression lock).
			name:    "macro-re-export-not-conflict",
			program: `(import (scheme base) (scheme r5rs)) (let ((x 1)) (cond (#t (+ x 1))))`,
			want:    "2",
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
				c.Assert(errors.Is(err, werr.ErrDuplicateBinding), qt.IsTrue)
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}
