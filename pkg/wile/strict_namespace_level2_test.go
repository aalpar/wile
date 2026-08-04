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
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/stdlib"
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
		{
			name: "let* and letrec",
			src:  `(let* ((x 1) (y x)) (letrec ((f (lambda (n) n))) (f y)))`,
			want: "1",
		},
		{
			// Named let is the self-tail-call shape, and it compiles to a jump
			// rather than a call to anything the registry would have supplied.
			name: "named let",
			src:  `(let loop ((i 0)) (if (if i #f #t) i (loop #f)))`,
			want: "#f",
		},
		{
			// set! on a LOCAL. There is no global to set at this level, so the
			// floor claim is specifically about the lexical case.
			name: "set! on a local",
			src:  `(let ((x 1)) (set! x 2) x)`,
			want: "2",
		},
		{
			name: "begin, quote, and define",
			src:  `(begin (define x '(a b)) x)`,
			want: "(a b)",
		},
		{
			name: "variadic lambda and internal define",
			src:  `((lambda args (define n args) n) 1 2)`,
			want: "(1 2)",
		},
		{
			name: "case-lambda",
			src:  `((case-lambda ((a) a) ((a b) b)) 1 2)`,
			want: "2",
		},
		{
			name: "cond-expand",
			src:  `(cond-expand (wile 1) (else 2))`,
			want: "1",
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

// TestNoAmbientBindingsCommutativity pins the property max-combining exists for:
// the narrowing options compose order-independently, and applying the weaker one
// after the stronger one does NOT widen the surface back out. Under
// last-one-wins, (WithoutAmbientBindings, WithStrictNamespace) would land on
// level 1 and quietly re-bind car — the one direction this family must never
// allow. (car '(1 2)) is the discriminator: bound at level 1, unbound at level 2.
func TestNoAmbientBindingsCommutativity(t *testing.T) {
	c := qt.New(t)

	orders := []struct {
		name string
		opts []wile.EngineOption
	}{
		{
			name: "no-ambient alone",
			opts: []wile.EngineOption{wile.WithoutAmbientBindings()},
		},
		{
			name: "strict then no-ambient",
			opts: []wile.EngineOption{wile.WithStrictNamespace(), wile.WithoutAmbientBindings()},
		},
		{
			name: "no-ambient then strict",
			opts: []wile.EngineOption{wile.WithoutAmbientBindings(), wile.WithStrictNamespace()},
		},
		{
			name: "no-ambient twice",
			opts: []wile.EngineOption{wile.WithoutAmbientBindings(), wile.WithoutAmbientBindings()},
		},
	}
	for _, o := range orders {
		t.Run(o.name, func(t *testing.T) {
			_, err := evalUnderProfile(t, wile.Small, `(car '(1 2))`, o.opts...)
			c.Assert(err, qt.IsNotNil,
				qt.Commentf("every order must resolve to level 2, where car is unbound"))
			c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
				qt.Commentf("want ErrNoSuchBinding, got: %v", err))
		})
	}
}

// TestNoAmbientBindingsOrthogonality confirms level 2 composes order-independently
// with WithProfile and WithSandbox, mirroring TestStrictNamespaceOrthogonality one
// level down: the level-2 surface holds in every order, and adding a sandbox layer
// does not change it.
//
// canImport is what the sandbox DOES change, and it is a separate axis: see
// TestNoAmbientBindingsSandboxIsConfinement.
func TestNoAmbientBindingsOrthogonality(t *testing.T) {
	c := qt.New(t)

	ctx := context.Background()
	orders := []struct {
		name      string
		opts      []wile.EngineOption
		canImport bool
	}{
		{
			name:      "profile,no-ambient",
			opts:      []wile.EngineOption{wile.WithProfile(wile.Small), wile.WithoutAmbientBindings()},
			canImport: true,
		},
		{
			name:      "no-ambient,profile",
			opts:      []wile.EngineOption{wile.WithoutAmbientBindings(), wile.WithProfile(wile.Small)},
			canImport: true,
		},
		{
			name:      "profile,no-ambient,sandbox",
			opts:      []wile.EngineOption{wile.WithProfile(wile.Small), wile.WithoutAmbientBindings(), wile.WithSandbox()},
			canImport: false,
		},
		{
			name:      "sandbox,no-ambient,profile",
			opts:      []wile.EngineOption{wile.WithSandbox(), wile.WithoutAmbientBindings(), wile.WithProfile(wile.Small)},
			canImport: false,
		},
	}
	for _, o := range orders {
		t.Run(o.name, func(t *testing.T) {
			opts := append([]wile.EngineOption{}, o.opts...)
			opts = append(opts, wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths())
			eng, err := wile.NewEngine(ctx, opts...)
			c.Assert(err, qt.IsNil)

			// The surface is the same in every order: not even core is visible.
			_, err = eng.EvalMultiple(ctx, `(car '(1 2))`)
			c.Assert(err, qt.IsNotNil)
			c.Assert(err.Error(), qt.Contains, "car")

			// The way back out is open iff the authorizer permits code:load.
			v, err := eng.EvalMultiple(ctx, `(import (scheme base)) (car '(1 2))`)
			if !o.canImport {
				c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
					qt.Commentf("sandbox denies code:load; want ErrAccessDenied, got: %v", err))
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(v.SchemeString(), qt.Equals, "1")
		})
	}
}

// TestNoAmbientBindingsSandboxIsConfinement records the one combination where the
// "strictness is explicitness, not confinement" framing stops holding.
//
// Level 2 withholds nothing permanently BECAUSE the program can import its way
// back to the whole profile. WithSandbox denies code:load, so it cannot. The two
// options are individually orthogonal — one picks the visible surface, the other
// picks the authorizer — but together they leave a program with the phase-handler
// floor and no route off it, which is confinement however it was reached. The same
// holds for WithProfile(Console)/ConsoleWithLoad, whose authorizers deny code:load
// on the stdlib path for the same reason.
//
// It is a real configuration and this test does not argue against it; it exists so
// the claim in WithoutAmbientBindings' doc comment is bounded by a test rather
// than by prose alone.
func TestNoAmbientBindingsSandboxIsConfinement(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.Small),
		wile.WithoutAmbientBindings(),
		wile.WithSandbox(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	// The floor is still there…
	v, err := eng.EvalMultiple(ctx, `(let ((x 1)) x)`)
	c.Assert(err, qt.IsNil)
	c.Assert(v.SchemeString(), qt.Equals, "1")

	// …and it is all there is: the import that would restore anything is denied.
	_, err = eng.EvalMultiple(ctx, `(import (scheme base))`)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("want code:load denied, got: %v", err))
}

// TestNoAmbientBindingsNoEscalation pins the security invariant one level down
// from TestStrictNamespaceNoEscalation: narrowing the visible surface must not
// widen the IMPORTABLE set. A KitchenSink-only library ((wile threads), backed by
// an extension Small never registers) stays unimportable under Small at level 2,
// and stays importable under KitchenSink. The profile is the boundary; strictness
// only decides what is pre-bound.
func TestNoAmbientBindingsNoEscalation(t *testing.T) {
	c := qt.New(t)

	t.Run("KitchenSink-only library still unimportable under Small", func(t *testing.T) {
		_, err := evalUnderProfile(t, wile.Small, `(import (wile threads)) 1`,
			wile.WithoutAmbientBindings())
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, werr.ErrFileNotFound), qt.IsTrue,
			qt.Commentf("level 2 must not widen the importable set; want ErrFileNotFound, got: %v", err))
	})

	t.Run("importable under KitchenSink at level 2", func(t *testing.T) {
		got, err := evalUnderProfile(t, wile.KitchenSink,
			`(import (scheme base)) (import (wile threads)) (procedure? make-thread)`,
			wile.WithoutAmbientBindings())
		c.Assert(err, qt.IsNil)
		c.Assert(got, qt.Equals, "#t")
	})
}

// TestNoAmbientBindingsRejectsCustomRegistry confirms the construction-time
// rejection covers the new option too. WithoutCore and level 2 look alike and are
// not the same thing: WithoutCore empties both the visible surface AND the
// registry backing library environments, level 2 empties only the visible surface.
// The engine refuses the pair rather than pick one reading.
func TestNoAmbientBindingsRejectsCustomRegistry(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	registryOpts := []struct {
		name string
		opt  wile.EngineOption
	}{
		{
			name: "WithoutCore",
			opt:  wile.WithoutCore(),
		},
		{
			name: "WithRegistry",
			opt:  wile.WithRegistry(registry.NewRegistry()),
		},
	}
	// Both orders: the rejection is a property of the config, not of which
	// option ran last.
	for _, tc := range registryOpts {
		t.Run(tc.name+",no-ambient", func(t *testing.T) {
			_, err := wile.NewEngine(ctx, tc.opt, wile.WithoutAmbientBindings())
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrEngineInit), qt.IsTrue,
				qt.Commentf("want ErrEngineInit, got: %v", err))
		})
		t.Run("no-ambient,"+tc.name, func(t *testing.T) {
			_, err := wile.NewEngine(ctx, wile.WithoutAmbientBindings(), tc.opt)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrEngineInit), qt.IsTrue,
				qt.Commentf("want ErrEngineInit, got: %v", err))
		})
	}
}

// TestNoAmbientBindingsAuthorizerStillGates confirms level 2 does not disable
// runtime authorization. The distinction matters more here than at level 1: level
// 2 withholds nothing permanently (the program imports its way back to the whole
// profile), so the authorizer is the only thing that actually denies. If narrowing
// the visible surface silently bypassed it, "strict" would read as confinement and
// not be any.
func TestNoAmbientBindingsAuthorizerStillGates(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	denyFile := security.AuthorizerFunc(func(req security.AccessRequest) error {
		if req.Resource == security.ResourceFile {
			return werr.WrapForeignErrorf(security.ErrAccessDenied, "denyFile: %s/%s", req.Resource, req.Action)
		}
		return nil
	})

	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.Small),
		wile.WithoutAmbientBindings(),
		wile.WithAuthorizer(denyFile),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(import (scheme file)) (open-input-file "/etc/hostname")`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("gated file op must be denied at level 2, got: %v", err))
}

// TestNoAmbientBindingsEngineInitAllProfiles records that level-2 engine
// CONSTRUCTION succeeds on all five profiles — the profiles differ only in what
// can be imported afterwards (§0.2: Small and KitchenSink can reach
// (scheme base), the other three cannot). Without this row it would be easy to
// read the Tiny/Console bound as an init failure and gate the option on profile,
// which would be the wrong fix for a pre-existing gap.
func TestNoAmbientBindingsEngineInitAllProfiles(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	profiles := []struct {
		name string
		p    wile.Profile
	}{
		{
			name: "Tiny",
			p:    wile.Tiny,
		},
		{
			name: "Console",
			p:    wile.Console,
		},
		{
			name: "ConsoleWithLoad",
			p:    wile.ConsoleWithLoad,
		},
		{
			name: "Small",
			p:    wile.Small,
		},
		{
			name: "KitchenSink",
			p:    wile.KitchenSink,
		},
	}
	for _, tc := range profiles {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := wile.NewEngine(ctx,
				wile.WithProfile(tc.p),
				wile.WithoutAmbientBindings(),
				wile.WithSourceFS(stdlib.FS),
				wile.WithLibraryPaths(),
			)
			c.Assert(err, qt.IsNil)

			// The phase-handler floor is the same on every profile.
			v, err := eng.EvalMultiple(ctx, `(let ((x 1)) x)`)
			c.Assert(err, qt.IsNil)
			c.Assert(v.SchemeString(), qt.Equals, "1")
		})
	}
}
