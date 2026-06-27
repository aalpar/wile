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

	"github.com/aalpar/wile/pkg/stdlib"
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
