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
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestNoAmbientBindingsFloor pins level 2's visible surface. The partition is
// three-way, not two-way: a form can be a phase handler and still be unusable,
// because its expansion calls a primitive the empty registry never bound. Each
// column fails differently and both dead columns pin their error TEXT, so a
// future change that moves a form between columns fails loudly rather than
// silently swapping one unbound name for another.
//
//	usable            phase handler whose codegen emits no call
//	resolves-unusable phase handler expanding through an unbound primitive
//	unresolved        bootstrap macro or primitive; both come from the registry
func TestNoAmbientBindingsFloor(t *testing.T) {
	c := qt.New(t)

	// Column 1: usable. These resolve AND run on an empty visible frame.
	usable := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "let",
			src:  `(let ((x 1)) x)`,
			want: "1",
		},
		{
			name: "lambda application",
			src:  `((lambda (x) x) 1)`,
			want: "1",
		},
		{
			name: "if",
			src:  `(if #t 1 2)`,
			want: "1",
		},
		{
			// A CONSTANT quasiquote template compiles to a literal and emits no
			// call. The moment an unquote appears it emits list; see below.
			name: "quasiquote, constant template",
			src:  "`(1 2)",
			want: "(1 2)",
		},
		{
			name: "define-syntax and use",
			src:  `(define-syntax m (syntax-rules () ((_ a) a))) (m 7)`,
			want: "7",
		},
	}
	for _, tc := range usable {
		t.Run("usable/"+tc.name, func(t *testing.T) {
			got, err := evalUnderProfile(t, wile.Small, tc.src, wile.WithoutAmbientBindings())
			c.Assert(err, qt.IsNil)
			c.Assert(got, qt.Equals, tc.want)
		})
	}

	// Columns 2 and 3: dead, with the offending name pinned. The wording differs
	// between them ("no such local or global binding" vs "no such binding … with
	// compatible scopes") because column 2 fails resolving a macro-introduced
	// reference against a scope set, and column 3 fails a plain lookup.
	dead := []struct {
		name   string
		src    string
		errsub string
	}{
		{
			name:   "resolves-unusable/quasiquote with unquote needs list",
			src:    "`(1 ,(if #t 2 3))",
			errsub: `no such local or global binding "list"`,
		},
		{
			name:   "resolves-unusable/unless needs not",
			src:    `(unless #f 1)`,
			errsub: `no such binding "not" with compatible scopes`,
		},
		{
			name:   "resolves-unusable/guard needs call-with-exit",
			src:    `(guard (e (#t 1)) 2)`,
			errsub: `no such binding "call-with-exit" with compatible scopes`,
		},
		{
			// cond is a bootstrap macro, carried by Registry.MacroSources(); an
			// empty registry supplies none.
			name:   "unresolved/cond is a bootstrap macro",
			src:    `(cond (#t 1))`,
			errsub: `no such local or global binding "cond"`,
		},
		{
			// when and unless land on OPPOSITE sides: when is a bootstrap macro,
			// unless is a phase handler that expands through not. The asymmetry
			// is pre-existing and out of scope; the two rows keep a future
			// grouping of them from asserting the wrong error for one.
			name:   "unresolved/when is a bootstrap macro",
			src:    `(when #t 1)`,
			errsub: `no such local or global binding "when"`,
		},
		{
			name:   "unresolved/core primitive",
			src:    `(car '(1 2))`,
			errsub: `no such local or global binding "car"`,
		},
	}
	for _, tc := range dead {
		t.Run(tc.name, func(t *testing.T) {
			_, err := evalUnderProfile(t, wile.Small, tc.src, wile.WithoutAmbientBindings())
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
				qt.Commentf("want ErrNoSuchBinding, got: %v", err))
			c.Assert(err.Error(), qt.Contains, tc.errsub)
		})
	}
}

// TestNoAmbientBindingsLayering is the point of the level: nothing is ambient,
// but nothing is withheld either — the program imports what it needs. Macros and
// procedures travel by different mechanisms (bootstrap MacroSources re-exported
// through the library's export list, versus primitive bindings resolved against
// the library env), so they are asserted separately; one combined assertion would
// hide which mechanism broke.
func TestNoAmbientBindingsLayering(t *testing.T) {
	c := qt.New(t)

	t.Run("derived syntax (cond) restored by import", func(t *testing.T) {
		got, err := evalUnderProfile(t, wile.Small,
			`(import (scheme base)) (cond ((= 1 1) 'yes) (else 'no))`,
			wile.WithoutAmbientBindings())
		c.Assert(err, qt.IsNil)
		c.Assert(got, qt.Equals, "yes")
	})

	t.Run("procedure (car) restored by import", func(t *testing.T) {
		got, err := evalUnderProfile(t, wile.Small,
			`(import (scheme base)) (car '(1 2))`,
			wile.WithoutAmbientBindings())
		c.Assert(err, qt.IsNil)
		c.Assert(got, qt.Equals, "1")
	})
}

// TestNoAmbientBindingsTinyBound pins the usability bound: level 2 is usable on
// Small and KitchenSink only. Under Tiny, (import (scheme base)) fails because
// base.sld exports 64 identifiers the Tiny registry never registers.
//
// This failure is PRE-EXISTING and reproduces identically on a non-strict engine
// — the second subtest is the control, and it exists so nobody "fixes" the first
// one inside the strict-namespace work. What level 2 changes is not the failure
// but the consequence: at level 0 or 1 Tiny still hands the program an ambient
// surface, so the broken import is an inconvenience; at level 2 there is no
// ambient surface and no way back.
func TestNoAmbientBindingsTinyBound(t *testing.T) {
	c := qt.New(t)

	t.Run("Tiny cannot import (scheme base) at level 2", func(t *testing.T) {
		_, err := evalUnderProfile(t, wile.Tiny, `(import (scheme base)) 1`,
			wile.WithoutAmbientBindings())
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, werr.ErrUnexportedIdentifier), qt.IsTrue,
			qt.Commentf("want ErrUnexportedIdentifier, got: %v", err))
	})

	t.Run("control: same failure on a non-strict Tiny engine", func(t *testing.T) {
		_, err := evalUnderProfile(t, wile.Tiny, `(import (scheme base)) 1`)
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, werr.ErrUnexportedIdentifier), qt.IsTrue,
			qt.Commentf("the Tiny export gap must be pre-existing, not a level-2 regression; got: %v", err))
	})
}
