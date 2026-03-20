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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/registry"
)

// TestNewEnvironmentTiny tests that the top-level environment can be created successfully.
func TestNewEnvironmentTiny(t *testing.T) {
	env, err := NewNamespaceFrameTiny(context.TODO())
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
