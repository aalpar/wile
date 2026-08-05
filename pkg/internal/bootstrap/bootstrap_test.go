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

package bootstrap

import (
	"context"
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/extensions/eval"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// TestNewEnvironmentTiny tests that the top-level environment can be created successfully.
func TestNewEnvironmentTiny(t *testing.T) {
	env, err := NewNamespaceFrame(context.TODO())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, env, qt.IsNotNil)
}

// TestNewTopLevelWithRegistry tests that the environment+registry variant returns
// both a valid environment and a populated registry.
func TestNewTopLevelWithRegistry(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	env, reg, err := NewTopLevelWithRegistry(ctx)
	c.Assert(err, qt.IsNil)
	c.Assert(env, qt.IsNotNil)
	c.Assert(reg, qt.IsNotNil)

	// Verify the registry has primitives (+ is registered by core)
	_, found := reg.FindPrimitive("+", 0)
	c.Assert(found, qt.IsTrue)
}

// TestSelectiveExtensionLoading verifies that passing an explicit extension
// list to initializeEnvironmentWithRegistry loads only those extensions.
func TestSelectiveExtensionLoading(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	topLevel := environment.NewNamespace()
	env := topLevel.Runtime()

	// Load only the math extension
	reg, err := initializeEnvironmentWithRegistry(ctx, env, []registry.Extension{math.Extension}, StrictOff)
	c.Assert(err, qt.IsNil)

	// Core primitives should still be present
	_, found := reg.FindPrimitive("+", 0)
	c.Assert(found, qt.IsTrue)

	// Math extension primitives should be present
	_, found = reg.FindPrimitive("sin", 0)
	c.Assert(found, qt.IsTrue)

	// Primitives from other extensions (io) should NOT be present
	_, found = reg.FindPrimitive("display", 0)
	c.Assert(found, qt.IsFalse)
}

// TestProfileExtensions exercises the profile→extensions dispatch table.
// Each named profile returns a deterministic, non-overlapping extension
// set; "kitchen-sink" returns a fresh copy so callers cannot mutate the
// underlying allExtensions slice.
func TestProfileExtensions(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name        string
		profile     string
		wantNonNil  bool
		wantMinSize int
	}{
		{"tiny returns nil", "tiny", false, 0},
		{"console returns 6 extensions", "console", true, 6},
		{"console-with-load returns 7 extensions", "console-with-load", true, 7},
		{"small returns 9 extensions", "small", true, 9},
		{"kitchen-sink returns full set", "kitchen-sink", true, 11},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			exts, err := ProfileExtensions(tc.profile)
			c.Assert(err, qt.IsNil)
			if tc.wantNonNil {
				c.Assert(exts, qt.IsNotNil)
				c.Assert(len(exts) >= tc.wantMinSize, qt.IsTrue,
					qt.Commentf("got %d extensions, want at least %d", len(exts), tc.wantMinSize))
			} else {
				c.Assert(exts, qt.IsNil)
			}
		})
	}
}

// TestProfileExtensions_Unknown verifies that unknown profile names yield
// a wrapped ErrUnknownProfile sentinel — the documented contract for the
// caller's errors.Is check.
func TestProfileExtensions_Unknown(t *testing.T) {
	c := qt.New(t)
	exts, err := ProfileExtensions("nonsense-profile-name")
	c.Assert(exts, qt.IsNil)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, ErrUnknownProfile), qt.IsTrue)
}

// TestProfileExtensions_KitchenSinkIsCopy verifies that mutating a returned
// kitchen-sink slice does not corrupt the package-level allExtensions slice
// for subsequent callers. This is the documented invariant of the case
// branch (see bootstrap.go:127-128).
func TestProfileExtensions_KitchenSinkIsCopy(t *testing.T) {
	c := qt.New(t)
	first, err := ProfileExtensions("kitchen-sink")
	c.Assert(err, qt.IsNil)
	originalLen := len(first)

	// Mutate the returned slice.
	first[0] = nil

	// A fresh call must still return the original, unmodified set.
	second, err := ProfileExtensions("kitchen-sink")
	c.Assert(err, qt.IsNil)
	c.Assert(len(second), qt.Equals, originalLen)
	c.Assert(second[0], qt.IsNotNil)
}

// TestNewProfileEnvironment verifies that a fresh namespace is produced from
// a parent and the requested extension set is loaded into it. The new
// namespace must share symbol interning with the parent (per R7RS §6.5)
// but have isolated bindings.
func TestNewProfileEnvironment(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	parent := environment.NewNamespace()

	// Build a profile with just the math extension to verify the extension
	// set is honored (sin should be present, display should not).
	exts := []registry.Extension{math.Extension}
	childNS, err := NewProfileEnvironment(ctx, parent, exts, StrictOff)
	c.Assert(err, qt.IsNil)
	c.Assert(childNS, qt.IsNotNil)
	c.Assert(childNS, qt.Not(qt.Equals), parent)

	reg, ok := childNS.Registry().(*registry.PrimitiveRegistry)
	c.Assert(ok, qt.IsTrue)
	c.Assert(reg, qt.IsNotNil)

	// Core primitives present.
	_, found := reg.FindPrimitive("+", 0)
	c.Assert(found, qt.IsTrue)

	// Math extension present.
	_, found = reg.FindPrimitive("sin", 0)
	c.Assert(found, qt.IsTrue)

	// Display (from io extension) NOT present.
	_, found = reg.FindPrimitive("display", 0)
	c.Assert(found, qt.IsFalse)
}

// TestNewProfileEnvironment_Empty verifies that passing a nil extension
// list (the "tiny" profile shape) still produces a working namespace with
// only core primitives.
func TestNewProfileEnvironment_Empty(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	parent := environment.NewNamespace()
	childNS, err := NewProfileEnvironment(ctx, parent, nil, StrictOff)
	c.Assert(err, qt.IsNil)
	c.Assert(childNS, qt.IsNotNil)

	reg, ok := childNS.Registry().(*registry.PrimitiveRegistry)
	c.Assert(ok, qt.IsTrue)
	// Core primitives still present even with no extensions.
	_, found := reg.FindPrimitive("+", 0)
	c.Assert(found, qt.IsTrue)

	// No extension primitives.
	_, found = reg.FindPrimitive("sin", 0)
	c.Assert(found, qt.IsFalse)
}

// TestNewProfileEnvironment_RecordsEffectiveRegistry pins the recording half of the
// strict-level seam. The namespace keeps the FULL registry so what a level withholds
// stays importable; EffectiveRegistry is the separate, honest report of what the
// visible top level was actually bound from. It must be recorded even when the two
// coincide, because pkg/wile reads an absent record as "nothing was narrowed" and
// falls back to the full registry — a read that fails OPEN.
func TestNewProfileEnvironment_RecordsEffectiveRegistry(t *testing.T) {
	ctx := context.Background()

	cases := []struct {
		name string
		// level narrows the visible top level.
		level StrictLevel
		// wantCore is whether a core primitive is part of the visible surface.
		wantCore bool
	}{
		{"off binds the whole registry", StrictOff, true},
		{"core binds only the core surface", StrictCore, true},
		{"no-bindings binds nothing", StrictNoBindings, false},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)

			parent := environment.NewNamespace()
			childNS, err := NewProfileEnvironment(ctx, parent, []registry.Extension{math.Extension}, tc.level)
			c.Assert(err, qt.IsNil)

			// The full registry is level-independent: sin stays importable at every level.
			full, ok := childNS.Registry().(*registry.PrimitiveRegistry)
			c.Assert(ok, qt.IsTrue)
			_, found := full.FindPrimitive("sin", 0)
			c.Assert(found, qt.IsTrue)

			eff, ok := childNS.EffectiveRegistry().(*registry.PrimitiveRegistry)
			c.Assert(ok, qt.IsTrue,
				qt.Commentf("no effective registry recorded: the narrowing is invisible and reads as unnarrowed"))
			c.Assert(eff, qt.IsNotNil)

			_, found = eff.FindPrimitive("car", 0)
			c.Assert(found, qt.Equals, tc.wantCore)

			// sin is extension-only, so it survives into the visible surface at
			// StrictOff alone — StrictCore withholds it just as StrictNoBindings does.
			_, found = eff.FindPrimitive("sin", 0)
			c.Assert(found, qt.Equals, tc.level == StrictOff)
		})
	}
}

// TestProfileFactoryCallback exercises the init() side effect that wires
// eval.ProfileFactory through to NewProfileEnvironment+ProfileExtensions.
// This is the path used by Scheme-level (environment '(wile <name>)).
func TestProfileFactoryCallback(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	c.Assert(eval.ProfileFactory, qt.IsNotNil)

	parent := environment.NewNamespace()

	t.Run("known profile constructs namespace", func(t *testing.T) {
		ns, err := eval.ProfileFactory(ctx, parent, "tiny", "")
		c.Assert(err, qt.IsNil)
		c.Assert(ns, qt.IsNotNil)
	})

	t.Run("unknown profile wraps ErrUnknownProfile", func(t *testing.T) {
		ns, err := eval.ProfileFactory(ctx, parent, "no-such-profile", "")
		c.Assert(ns, qt.IsNil)
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, ErrUnknownProfile), qt.IsTrue)
	})

	// Strictness is validated on this side of the ProfileFactory boundary (eval
	// forwards the raw spelling), so a bad level must reach ErrUnknownStrictLevel
	// here rather than constructing a silently-unnarrowed namespace. Constructing a
	// namespace is not evidence the level survived the trip, so each row probes the
	// visible top level. Two probes, because neither alone separates all three
	// levels: car (core) distinguishes no-bindings, sin (math, an extension of the
	// small profile) distinguishes core.
	levels := []struct {
		level    string
		wantCore bool
		wantExt  bool
	}{
		{"", true, true},
		{"core", true, false},
		{"no-bindings", false, false},
	}
	for _, tc := range levels {
		t.Run("strict level "+tc.level+" constructs namespace", func(t *testing.T) {
			c := qt.New(t)
			ns, err := eval.ProfileFactory(ctx, parent, "small", tc.level)
			c.Assert(err, qt.IsNil)
			c.Assert(ns, qt.IsNotNil)

			carBinding := ns.Runtime().GetBinding(values.NewSymbol("car"), syntax.AllScopes())
			c.Assert(carBinding != nil, qt.Equals, tc.wantCore,
				qt.Commentf("level %q: car bound = %v, want %v", tc.level, carBinding != nil, tc.wantCore))

			sinBinding := ns.Runtime().GetBinding(values.NewSymbol("sin"), syntax.AllScopes())
			c.Assert(sinBinding != nil, qt.Equals, tc.wantExt,
				qt.Commentf("level %q: sin bound = %v, want %v", tc.level, sinBinding != nil, tc.wantExt))
		})
	}

	t.Run("unknown strict level wraps ErrUnknownStrictLevel", func(t *testing.T) {
		ns, err := eval.ProfileFactory(ctx, parent, "small", "bare")
		c.Assert(ns, qt.IsNil)
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, ErrUnknownStrictLevel), qt.IsTrue)
	})
}

// TestParseStrictLevel pins the level vocabulary shared by cmd/wile's --strict,
// pkg/wile's two options, and (environment '(wile <name> <strictness>)). The empty
// string is the ABSENT case, not an error: tryWileProfile forwards "" when the
// spec omits the element, so mapping it to StrictOff is what keeps that caller
// free of a special case.
func TestParseStrictLevel(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name    string
		want    StrictLevel
		wantErr bool
	}{
		{
			name: "",
			want: StrictOff,
		},
		{
			name: "core",
			want: StrictCore,
		},
		{
			name: "no-bindings",
			want: StrictNoBindings,
		},
		{
			name:    "bare",
			wantErr: true,
		},
		{
			name:    "Core",
			wantErr: true,
		},
	}
	for _, tc := range cases {
		t.Run("level "+tc.name, func(t *testing.T) {
			got, err := ParseStrictLevel(tc.name)
			if tc.wantErr {
				c.Assert(errors.Is(err, ErrUnknownStrictLevel), qt.IsTrue,
					qt.Commentf("want ErrUnknownStrictLevel, got: %v", err))
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

// TestCharsetsInProfileExtensions verifies that the charsets extension is included
// in each profile that provides standard library coverage (small, console, kitchen-sink).
func TestCharsetsInProfileExtensions(t *testing.T) {
	tests := []struct {
		name    string
		profile string
	}{
		{"small includes charsets", "small"},
		{"console includes charsets", "console"},
		{"kitchen-sink includes charsets", "kitchen-sink"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			exts, err := ProfileExtensions(tc.profile)
			c.Assert(err, qt.IsNil)
			found := false
			for _, ext := range exts {
				if ext.Name() == "charsets" {
					found = true
					break
				}
			}
			c.Assert(found, qt.IsTrue, qt.Commentf("profile %s missing charsets extension", tc.profile))
		})
	}
}

// TestCharsetsExcludedFromTiny verifies that the minimal "tiny" profile has no
// extensions at all — charsets must not appear there.
func TestCharsetsExcludedFromTiny(t *testing.T) {
	tests := []struct {
		name    string
		profile string
	}{
		{"tiny has no extensions", "tiny"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			exts, err := ProfileExtensions(tc.profile)
			c.Assert(err, qt.IsNil)
			c.Assert(exts, qt.IsNil) // tiny has no extensions
		})
	}
}
