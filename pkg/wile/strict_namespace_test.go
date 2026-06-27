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

	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// evalUnderProfile builds an engine with the given profile and extra options
// (plus the stdlib FS so (scheme …) libraries resolve) and evaluates src,
// returning the result's SchemeString and any error. It is the shared harness
// for the strict-namespace suite across this file.
func evalUnderProfile(t *testing.T, p wile.Profile, src string, extra ...wile.EngineOption) (string, error) {
	t.Helper()
	ctx := context.Background()
	opts := []wile.EngineOption{
		wile.WithProfile(p),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	}
	opts = append(opts, extra...)
	eng, err := wile.NewEngine(ctx, opts...)
	if err != nil {
		return "", err
	}
	v, err := eng.EvalMultiple(ctx, src)
	if err != nil {
		return "", err
	}
	return v.SchemeString(), nil
}

// TestStrictNamespaceBaseline characterizes the PRE-change behavior that
// strict-namespace mode will alter. Under a non-strict Small profile, the
// profile's extension primitives are pre-bound at the top level (display is
// visible without import) and (scheme r5rs) imports cleanly. Phase 2/3 of the
// implementation plan add the strict-mode rows that make display require an
// explicit import while keeping r5rs importable over a bare baseline.
//
// These rows pass today; they are the oracle the strict-mode delta is measured
// against (impl plan Phase 0).
func TestStrictNamespaceBaseline(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "core primitive visible without import",
			src:  `(car '(1 2))`,
			want: "1",
		},
		{
			name: "extension primitive (display) visible without import under non-strict Small",
			src:  `(procedure? display)`,
			want: "#t",
		},
		{
			name: "scheme r5rs importable under Small",
			src:  `(import (scheme r5rs)) (exact->inexact 1/2)`,
			want: "0.5",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalUnderProfile(t, wile.Small, tc.src)
			c.Assert(err, qt.IsNil)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

// TestStrictNamespaceBareSurface pins the strict-mode top-level surface: core
// primitives and the define/syntax machinery stay visible, while a profile's
// extension primitive (display) is NOT pre-bound and must be imported.
func TestStrictNamespaceBareSurface(t *testing.T) {
	c := qt.New(t)

	okCases := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "core primitive still visible",
			src:  `(car '(1 2))`,
			want: "1",
		},
		{
			name: "core arithmetic still visible",
			src:  `(+ 1 2)`,
			want: "3",
		},
		{
			name: "define and reference still work",
			src:  `(define x 3) x`,
			want: "3",
		},
	}
	for _, tc := range okCases {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalUnderProfile(t, wile.Small, tc.src, wile.WithStrictNamespace())
			c.Assert(err, qt.IsNil)
			c.Assert(got, qt.Equals, tc.want)
		})
	}

	t.Run("extension primitive (display) NOT visible without import", func(t *testing.T) {
		_, err := evalUnderProfile(t, wile.Small, `(procedure? display)`, wile.WithStrictNamespace())
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Contains, "display")
	})
}

// TestStrictNamespaceR5RSOnBare is the headline goal: over a bare strict top
// level, (import (scheme r5rs)) resolves and layers the R5RS surface on top.
func TestStrictNamespaceR5RSOnBare(t *testing.T) {
	c := qt.New(t)

	t.Run("import (scheme r5rs) layers the R5RS surface", func(t *testing.T) {
		got, err := evalUnderProfile(t, wile.Small,
			`(import (scheme r5rs))
			 (list (exact->inexact 1/2) (force (delay 7)) (call-with-current-continuation (lambda (k) 9)))`,
			wile.WithStrictNamespace())
		c.Assert(err, qt.IsNil)
		c.Assert(got, qt.Equals, "(0.5 7 9)")
	})

	t.Run("extension primitive reachable after explicit import", func(t *testing.T) {
		got, err := evalUnderProfile(t, wile.Small,
			`(import (scheme write)) (procedure? display)`, wile.WithStrictNamespace())
		c.Assert(err, qt.IsNil)
		c.Assert(got, qt.Equals, "#t")
	})

	// Re-import identity (design D2 edge): importing (scheme base) re-introduces
	// names that are also visible as core primitives. Import writes to the mutable
	// global while core lives in the parent sealed base, so this must NOT raise
	// ErrDuplicateBinding.
	t.Run("re-import of core names does not conflict", func(t *testing.T) {
		got, err := evalUnderProfile(t, wile.Small,
			`(import (scheme base)) (car '(1 2))`, wile.WithStrictNamespace())
		c.Assert(err, qt.IsNil)
		c.Assert(got, qt.Equals, "1")
	})

	// Imported procedure behaves under the default immutable-top-level /
	// frame-reclaim regime when driven in a loop.
	t.Run("imported procedure works in a loop", func(t *testing.T) {
		got, err := evalUnderProfile(t, wile.Small,
			`(import (scheme r5rs))
			 (let loop ((i 0) (acc 0.0))
			   (if (= i 100) acc (loop (+ i 1) (+ acc (exact->inexact i)))))`,
			wile.WithStrictNamespace())
		c.Assert(err, qt.IsNil)
		c.Assert(got, qt.Equals, "4950.0")
	})
}

// TestStrictNamespaceNoEscalation pins the security invariant: strict mode does
// NOT widen the importable set beyond the profile. A KitchenSink-only library
// ((wile threads), backed by the threads extension absent from Small) is
// unimportable under Small whether or not strict mode is on, and importable
// under KitchenSink. The profile remains the boundary; strict only changes
// top-level visibility, never reachability.
func TestStrictNamespaceNoEscalation(t *testing.T) {
	c := qt.New(t)

	t.Run("KitchenSink-only library unimportable under Small (non-strict)", func(t *testing.T) {
		_, err := evalUnderProfile(t, wile.Small, `(import (wile threads)) (procedure? make-thread)`)
		c.Assert(err, qt.IsNotNil)
	})

	t.Run("KitchenSink-only library still unimportable under Small + strict", func(t *testing.T) {
		_, err := evalUnderProfile(t, wile.Small,
			`(import (wile threads)) (procedure? make-thread)`, wile.WithStrictNamespace())
		c.Assert(err, qt.IsNotNil)
	})

	t.Run("importable under KitchenSink + strict (boundary is the profile, not strict)", func(t *testing.T) {
		got, err := evalUnderProfile(t, wile.KitchenSink,
			`(import (wile threads)) (procedure? make-thread)`, wile.WithStrictNamespace())
		c.Assert(err, qt.IsNil)
		c.Assert(got, qt.Equals, "#t")
	})
}

// TestStrictNamespaceOrthogonality confirms WithStrictNamespace composes
// order-independently with WithProfile and WithSandbox: the bare surface holds
// regardless of option order and is unaffected by adding a sandbox layer.
func TestStrictNamespaceOrthogonality(t *testing.T) {
	c := qt.New(t)

	ctx := context.Background()
	orders := []struct {
		name string
		opts []wile.EngineOption
	}{
		{
			name: "profile,strict",
			opts: []wile.EngineOption{wile.WithProfile(wile.Small), wile.WithStrictNamespace()},
		},
		{
			name: "strict,profile",
			opts: []wile.EngineOption{wile.WithStrictNamespace(), wile.WithProfile(wile.Small)},
		},
		{
			name: "profile,strict,sandbox",
			opts: []wile.EngineOption{wile.WithProfile(wile.Small), wile.WithStrictNamespace(), wile.WithSandbox()},
		},
		{
			name: "sandbox,strict,profile",
			opts: []wile.EngineOption{wile.WithSandbox(), wile.WithStrictNamespace(), wile.WithProfile(wile.Small)},
		},
	}
	for _, o := range orders {
		t.Run(o.name, func(t *testing.T) {
			opts := append([]wile.EngineOption{}, o.opts...)
			opts = append(opts, wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths())
			eng, err := wile.NewEngine(ctx, opts...)
			c.Assert(err, qt.IsNil)

			// core visible
			v, err := eng.EvalMultiple(ctx, `(car '(1 2))`)
			c.Assert(err, qt.IsNil)
			c.Assert(v.SchemeString(), qt.Equals, "1")

			// extension primitive not visible until imported (bare surface holds)
			_, err = eng.EvalMultiple(ctx, `(procedure? display)`)
			c.Assert(err, qt.IsNotNil)
			c.Assert(err.Error(), qt.Contains, "display")
		})
	}
}

// TestStrictNamespaceAuthorizerStillGates confirms strict mode (a visibility
// change) does not disable runtime authorization: an explicit deny authorizer
// still rejects a gated operation under a strict engine.
func TestStrictNamespaceAuthorizerStillGates(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	denyAll := security.AuthorizerFunc(func(req security.AccessRequest) error {
		return werr.WrapForeignErrorf(security.ErrAccessDenied, "denyAll: %s/%s", req.Resource, req.Action)
	})

	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.Small),
		wile.WithStrictNamespace(),
		wile.WithAuthorizer(denyAll),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	// Import the file library, then a gated file op must be denied (not silently
	// allowed) — proving the authorizer chain survives strict mode.
	_, err = eng.EvalMultiple(ctx, `(import (scheme file)) (open-input-file "/etc/hostname")`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("gated file op must be denied under strict mode, got: %v", err))
}

// TestStrictNamespaceTinyParity confirms the strict bare surface equals a Tiny
// engine's visible surface (design D2): the same snippet yields the same verdict
// under Tiny and under Small+WithStrictNamespace, for a core op (both succeed)
// and for an extension op (both fail unbound).
func TestStrictNamespaceTinyParity(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name    string
		src     string
		wantErr bool
		want    string
	}{
		{
			name:    "core op succeeds under both",
			src:     `(car '(1 2))`,
			wantErr: false,
			want:    "1",
		},
		{
			name:    "extension op unbound under both",
			src:     `(procedure? display)`,
			wantErr: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			tinyVal, tinyErr := evalUnderProfile(t, wile.Tiny, tc.src)
			strictVal, strictErr := evalUnderProfile(t, wile.Small, tc.src, wile.WithStrictNamespace())
			if tc.wantErr {
				c.Assert(tinyErr, qt.IsNotNil)
				c.Assert(strictErr, qt.IsNotNil)
				return
			}
			c.Assert(tinyErr, qt.IsNil)
			c.Assert(strictErr, qt.IsNil)
			c.Assert(tinyVal, qt.Equals, tc.want)
			c.Assert(strictVal, qt.Equals, tinyVal)
		})
	}
}
