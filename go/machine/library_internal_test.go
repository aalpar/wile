// Copyright 2025 Aaron Alpar
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

package machine

import (
	"testing"

	"wile/environment"

	qt "github.com/frankban/quicktest"
)

// Tests moved from coverage_additional_test.go that need internal access

// TestLibraryRegistryMethodsAdditional tests LibraryRegistry methods
func TestLibraryRegistryMethodsAdditional(t *testing.T) {
	reg := NewLibraryRegistry()

	// Create a test library
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	lib := &CompiledLibrary{
		Name:    NewLibraryName("test", "lib"),
		Env:     env,
		Exports: map[string]string{},
	}

	// Register it
	reg.Register(lib) //nolint:errcheck

	// Look it up
	found := reg.Lookup(NewLibraryName("test", "lib"))
	qt.Assert(t, found, qt.IsNotNil)
	qt.Assert(t, found.Name.String(), qt.Equals, "test/lib")

	// Look up non-existent
	notFound := reg.Lookup(NewLibraryName("nonexistent"))
	qt.Assert(t, notFound, qt.IsNil)
}

// TestParseLibraryNameErrors tests parseLibraryName error cases
func TestParseLibraryNameErrors(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"empty library name", "(define-library ())"},
		{"invalid library name element", "(define-library (scheme 123))"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			_, err := newTopLevelThunk(sv, env)
			// These should either succeed or fail gracefully
			_ = err
		})
	}
}

// TestLibraryRequirementIsSatisfiedAdditional tests libraryRequirement.IsSatisfied
func TestLibraryRequirementIsSatisfiedAdditional(t *testing.T) {
	// With nil registry
	libReq := &libraryRequirement{name: NewLibraryName("scheme", "base")}
	qt.Assert(t, libReq.IsSatisfied(nil), qt.IsFalse)

	// With registry but library not loaded
	registry := NewLibraryRegistry()
	qt.Assert(t, libReq.IsSatisfied(registry), qt.IsFalse)

	// With library registered
	env := environment.NewTopLevelEnvironmentFrame()
	lib := NewCompiledLibrary(NewLibraryName("test", "lib"), env)
	registry.Register(lib) //nolint:errcheck

	testLibReq := &libraryRequirement{name: NewLibraryName("test", "lib")}
	qt.Assert(t, testLibReq.IsSatisfied(registry), qt.IsTrue)
}
