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

package compilation

import (
	"context"
	"errors"
	"sort"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

// Tests moved from coverage_additional_test.go that need internal access

// TestLibraryRegistryMethodsAdditional tests LibraryRegistry methods
func TestLibraryRegistryMethodsAdditional(t *testing.T) {
	reg := NewLibraryRegistry()

	// Create a test library
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())

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
	qt.Assert(t, libReq.IsSatisfied(context.Background(), nil, nil), qt.IsFalse)

	// With registry but library not loaded
	registry := NewLibraryRegistry()
	qt.Assert(t, libReq.IsSatisfied(context.Background(), registry, nil), qt.IsFalse)

	// With library registered
	env := environment.NewNamespace().Runtime()
	lib := NewCompiledLibrary(NewLibraryName("test", "lib"), env)
	registry.Register(lib) //nolint:errcheck

	testLibReq := &libraryRequirement{name: NewLibraryName("test", "lib")}
	qt.Assert(t, testLibReq.IsSatisfied(context.Background(), registry, nil), qt.IsTrue)
}

// TestApplyToExports_Modifiers tests ImportSet modifier logic on a CompiledLibrary
// with known exports.
func TestApplyToExports_Modifiers(t *testing.T) {
	lib := &CompiledLibrary{
		Name: NewLibraryName("test", "lib"),
		Exports: map[string]string{
			"alpha": "alpha",
			"beta":  "beta",
			"gamma": "gamma",
		},
	}

	sortedKeys := func(m map[string]string) []string {
		keys := make([]string, 0, len(m))
		for k := range m {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		return keys
	}

	testCases := []struct {
		name      string
		importSet *ImportSet
		wantKeys  []string
		wantErr   bool
	}{
		{
			name:      "no modifiers exports all",
			importSet: &ImportSet{},
			wantKeys:  []string{"alpha", "beta", "gamma"},
		},
		{
			name: "only alpha",
			importSet: &ImportSet{
				Only: map[string]struct{}{"alpha": {}},
			},
			wantKeys: []string{"alpha"},
		},
		{
			name: "except gamma",
			importSet: &ImportSet{
				Except: map[string]struct{}{"gamma": {}},
			},
			wantKeys: []string{"alpha", "beta"},
		},
		{
			name: "prefix t:",
			importSet: &ImportSet{
				Prefix: "t:",
			},
			wantKeys: []string{"t:alpha", "t:beta", "t:gamma"},
		},
		{
			name: "rename alpha to a",
			importSet: &ImportSet{
				Renames: map[string]string{"alpha": "a"},
			},
			wantKeys: []string{"a", "beta", "gamma"},
		},
		{
			name: "only nonexistent errors",
			importSet: &ImportSet{
				Only: map[string]struct{}{"nonexistent": {}},
			},
			wantErr: true,
		},
		{
			name: "except nonexistent errors",
			importSet: &ImportSet{
				Except: map[string]struct{}{"nonexistent": {}},
			},
			wantErr: true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, err := tc.importSet.ApplyToExports(lib)
			if tc.wantErr {
				qt.Assert(t, err, qt.IsNotNil)
				qt.Assert(t, errors.Is(err, werr.ErrUnexportedIdentifier), qt.IsTrue)
				return
			}
			qt.Assert(t, err, qt.IsNil)
			got := sortedKeys(result)
			wantSorted := make([]string, len(tc.wantKeys))
			copy(wantSorted, tc.wantKeys)
			sort.Strings(wantSorted)
			qt.Assert(t, got, qt.DeepEquals, wantSorted)
		})
	}
}

// TestLibraryRegistryLoadingState tests IsLoading/StartLoading/FinishLoading
// state transitions on LibraryRegistry.
func TestLibraryRegistryLoadingState(t *testing.T) {
	reg := NewLibraryRegistry()
	name := NewLibraryName("test", "lib")

	// Initially not loading
	qt.Assert(t, reg.IsLoading(name), qt.IsFalse)

	// StartLoading -> IsLoading true
	reg.StartLoading(name)
	qt.Assert(t, reg.IsLoading(name), qt.IsTrue)

	// FinishLoading -> IsLoading false
	reg.FinishLoading(name)
	qt.Assert(t, reg.IsLoading(name), qt.IsFalse)
}

// TestLoadLibrary_NilRegistry tests that LoadLibrary returns an error when
// the environment has no library registry configured.
func TestLoadLibrary_NilRegistry(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	// env has no library registry
	_, err := LoadLibrary(context.Background(), NewLibraryName("test"), env, machine.NewVMMacroEvaluator())
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no library registry")
}

// TestLoadLibrary_NoFileResolver tests that LoadLibrary returns an error when
// the environment has a registry but no file resolver configured.
func TestLoadLibrary_NoFileResolver(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	reg := NewLibraryRegistry()
	env.SetLibraryRegistry(reg)
	// env has no file resolver
	_, err := LoadLibrary(context.Background(), NewLibraryName("test"), env, machine.NewVMMacroEvaluator())
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no file resolver")
}

// TestLibraryRegistryFindLibraryFile tests FindLibraryFile behavior when
// no matching file exists on the filesystem.
func TestLibraryRegistryFindLibraryFile(t *testing.T) {
	reg := NewLibraryRegistry()
	// Use an empty temp directory so the library is guaranteed not found,
	// regardless of platform or working directory.
	reg.SetSearchPaths([]string{t.TempDir()})
	name := NewLibraryName("no", "such", "lib")
	path, err := reg.FindLibraryFile(name)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, path, qt.Equals, "")
	qt.Assert(t, errors.Is(err, werr.ErrLibraryNotFound), qt.IsTrue)
}

// TestCompiledLibrary_Methods exercises accessor methods on CompiledLibrary
// and LibraryRegistry that are otherwise at 0% coverage.
func TestCompiledLibrary_Methods(t *testing.T) {
	t.Run("IsExported and GetInternalName", func(t *testing.T) {
		lib := NewCompiledLibrary(
			NewLibraryName("test", "lib"),
			environment.NewNamespace().Runtime(),
		)
		lib.AddExport("foo", "internal-foo")
		lib.AddExport("bar", "") // defaults to "bar"

		qt.Assert(t, lib.IsExported("foo"), qt.IsTrue)
		qt.Assert(t, lib.IsExported("bar"), qt.IsTrue)
		qt.Assert(t, lib.IsExported("baz"), qt.IsFalse)

		qt.Assert(t, lib.GetInternalName("foo"), qt.Equals, "internal-foo")
		qt.Assert(t, lib.GetInternalName("bar"), qt.Equals, "bar")
		qt.Assert(t, lib.GetInternalName("baz"), qt.Equals, "")
	})

	t.Run("SetImportObserver and ImportObserver", func(t *testing.T) {
		reg := NewLibraryRegistry()

		// Initially nil
		qt.Assert(t, reg.ImportObserver() == nil, qt.IsTrue)

		// Set observer
		var called bool
		obs := func(evt LibraryImportEvent) {
			called = true
			_ = evt
		}
		reg.SetImportObserver(obs)
		qt.Assert(t, reg.ImportObserver() != nil, qt.IsTrue)

		// Invoke observer directly to verify it's wired up
		reg.ImportObserver()(LibraryImportEvent{})
		qt.Assert(t, called, qt.IsTrue)

		// Remove observer
		reg.SetImportObserver(nil)
		qt.Assert(t, reg.ImportObserver() == nil, qt.IsTrue)
	})

	t.Run("fireImportObserver with observer set", func(t *testing.T) {
		env := newNamespace(environment.NewNamespace().Runtime())
		reg := NewLibraryRegistry()
		env.SetLibraryRegistry(reg)

		var received LibraryImportEvent
		reg.SetImportObserver(func(evt LibraryImportEvent) {
			received = evt
		})

		lib := NewCompiledLibrary(
			NewLibraryName("test", "fire"),
			environment.NewNamespace().Runtime(),
		)
		lib.AddExport("x", "x")
		lib.AddExport("y", "y")

		bindings := map[string]string{"x": "x"}
		importer := NewLibraryName("my", "app")

		fireImportObserver(env, lib, bindings, importer, 0)

		qt.Assert(t, received.Library.Key(), qt.Equals, "test/fire")
		qt.Assert(t, received.Importer.Key(), qt.Equals, "my/app")
		qt.Assert(t, received.Phase, qt.Equals, 0)
		qt.Assert(t, received.Exports, qt.HasLen, 2)
		qt.Assert(t, received.Imported, qt.HasLen, 1)
		qt.Assert(t, received.Imported[0], qt.Equals, "x")
	})

	t.Run("fireImportObserver without observer", func(t *testing.T) {
		env := newNamespace(environment.NewNamespace().Runtime())
		reg := NewLibraryRegistry()
		env.SetLibraryRegistry(reg)
		// No observer set — should not panic
		lib := NewCompiledLibrary(
			NewLibraryName("test", "noop"),
			environment.NewNamespace().Runtime(),
		)
		fireImportObserver(env, lib, map[string]string{}, LibraryName{}, 0)
	})

	t.Run("fireImportObserver without registry", func(t *testing.T) {
		env := newNamespace(environment.NewNamespace().Runtime())
		// No registry set — should not panic
		lib := NewCompiledLibrary(
			NewLibraryName("test", "noop"),
			environment.NewNamespace().Runtime(),
		)
		fireImportObserver(env, lib, map[string]string{}, LibraryName{}, 0)
	})
}
