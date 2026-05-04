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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/extensions/eval"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/registry"
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
	reg, err := initializeEnvironmentWithRegistry(ctx, env, []registry.Extension{math.Extension})
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
	childNS, err := NewProfileEnvironment(ctx, parent, exts)
	c.Assert(err, qt.IsNil)
	c.Assert(childNS, qt.IsNotNil)
	c.Assert(childNS, qt.Not(qt.Equals), parent)

	reg, ok := childNS.Registry().(*registry.Registry)
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
	childNS, err := NewProfileEnvironment(ctx, parent, nil)
	c.Assert(err, qt.IsNil)
	c.Assert(childNS, qt.IsNotNil)

	reg, ok := childNS.Registry().(*registry.Registry)
	c.Assert(ok, qt.IsTrue)
	// Core primitives still present even with no extensions.
	_, found := reg.FindPrimitive("+", 0)
	c.Assert(found, qt.IsTrue)

	// No extension primitives.
	_, found = reg.FindPrimitive("sin", 0)
	c.Assert(found, qt.IsFalse)
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
		ns, err := eval.ProfileFactory(ctx, parent, "tiny")
		c.Assert(err, qt.IsNil)
		c.Assert(ns, qt.IsNotNil)
	})

	t.Run("unknown profile wraps ErrUnknownProfile", func(t *testing.T) {
		ns, err := eval.ProfileFactory(ctx, parent, "no-such-profile")
		c.Assert(ns, qt.IsNil)
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, ErrUnknownProfile), qt.IsTrue)
	})
}

// TestCharsetsInProfileExtensions verifies that the charsets extension is included
// in each profile that provides standard library coverage (small, console, kitchen-sink).
func TestCharsetsInProfileExtensions(t *testing.T) {
	c := qt.New(t)
	for _, profile := range []string{"small", "console", "kitchen-sink"} {
		exts, err := ProfileExtensions(profile)
		c.Assert(err, qt.IsNil)
		found := false
		for _, ext := range exts {
			if ext.Name() == "charsets" {
				found = true
				break
			}
		}
		c.Assert(found, qt.IsTrue, qt.Commentf("profile %s missing charsets extension", profile))
	}
}

// TestCharsetsExcludedFromTiny verifies that the minimal "tiny" profile has no
// extensions at all — charsets must not appear there.
func TestCharsetsExcludedFromTiny(t *testing.T) {
	c := qt.New(t)
	exts, err := ProfileExtensions("tiny")
	c.Assert(err, qt.IsNil)
	c.Assert(exts, qt.IsNil) // tiny has no extensions
}
