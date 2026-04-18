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

	qt "github.com/frankban/quicktest"
)

// TestExtensionListConsistency verifies that WithProfile(KitchenSink) and
// bootstrap's allExtensions stay in sync. Both lists must register the same
// set of (wile <name>) extension libraries. If this test fails after adding
// a new extension, update BOTH lists: profile.go KitchenSink.extensions() and
// internal/bootstrap/environment_tiny.go allExtensions.
func TestExtensionListConsistency(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
	c.Assert(err, qt.IsNil)

	libs, err := eng.AvailableLibraries(ctx)
	c.Assert(err, qt.IsNil)

	// Extract (wile <name>) libraries from the engine.
	wileLibs := make(map[string]bool)
	for _, lib := range libs {
		if len(lib.Parts) == 2 && lib.Parts[0] == "wile" {
			wileLibs[lib.Parts[1]] = true
		}
	}

	// Every extension must produce a (wile <name>) library.
	// If this list needs updating, both KitchenSink.extensions() in profile.go
	// and allExtensions in internal/bootstrap/bootstrap.go must be updated
	// together.
	expected := []string{
		"io", "files", "math", "introspection", "eval",
		"namespace", "threads", "gointerop", "all", "system", "process", "envvars",
	}
	for _, name := range expected {
		c.Assert(wileLibs[name], qt.IsTrue,
			qt.Commentf("missing (wile %s) library", name))
	}

	// Verify KitchenSink profile registers exactly the expected count.
	c.Assert(len(KitchenSink.extensions()), qt.Equals, len(expected),
		qt.Commentf("KitchenSink.extensions() count mismatch: want %d, got %d", len(expected), len(KitchenSink.extensions())))
}
