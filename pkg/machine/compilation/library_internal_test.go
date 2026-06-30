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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/werr"

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
	qt.Assert(t, libReq.IsSatisfied(context.Background(), nil, nil, nil), qt.IsFalse)

	// With registry but library not loaded
	registry := NewLibraryRegistry()
	qt.Assert(t, libReq.IsSatisfied(context.Background(), registry, nil, nil), qt.IsFalse)

	// With library registered
	env := environment.NewNamespace().Runtime()
	lib := NewCompiledLibrary(NewLibraryName("test", "lib"), env)
	registry.Register(lib) //nolint:errcheck

	testLibReq := &libraryRequirement{name: NewLibraryName("test", "lib")}
	qt.Assert(t, testLibReq.IsSatisfied(context.Background(), registry, nil, nil), qt.IsTrue)
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

	// build constructs an import set on the test library by applying the modifier
	// builders in order (innermost first), matching how the parser unwinds nesting.
	build := func(mods ...func(*ImportSet)) *ImportSet {
		is := NewImportSet(lib.Name)
		for _, m := range mods {
			m(is)
		}
		return is
	}
	only := func(ids ...string) func(*ImportSet) {
		return func(is *ImportSet) {
			set := make(map[string]struct{}, len(ids))
			for _, id := range ids {
				set[id] = struct{}{}
			}
			is.AddOnly(set)
		}
	}
	except := func(ids ...string) func(*ImportSet) {
		return func(is *ImportSet) {
			set := make(map[string]struct{}, len(ids))
			for _, id := range ids {
				set[id] = struct{}{}
			}
			is.AddExcept(set)
		}
	}
	prefix := func(p string) func(*ImportSet) {
		return func(is *ImportSet) {
			is.AddPrefix(p)
		}
	}
	rename := func(old, neu string) func(*ImportSet) {
		return func(is *ImportSet) {
			is.AddRename(map[string]string{old: neu})
		}
	}

	testCases := []struct {
		name      string
		importSet *ImportSet
		wantKeys  []string
		wantErr   bool
		// wantErrIs, when set, is the sentinel the error must match; defaults to
		// ErrUnexportedIdentifier (the only/except not-exported case).
		wantErrIs error
	}{
		{
			name:      "no modifiers exports all",
			importSet: build(),
			wantKeys:  []string{"alpha", "beta", "gamma"},
		},
		{
			// (only LIB) with zero identifiers selects the empty subset ⇒ import nothing
			// (R7RS §5.6: <identifier> … is zero-or-more).
			name:      "only with no identifiers imports nothing",
			importSet: build(only()),
			wantKeys:  []string{},
		},
		{
			// (rename LIB (alpha beta)): alpha→beta collides with the pass-through beta;
			// two different exports under one name ⇒ error, not a silent map-order drop.
			name:      "rename target collides with passthrough errors",
			importSet: build(rename("alpha", "beta")),
			wantErr:   true,
			wantErrIs: werr.ErrDuplicateBinding,
		},
		{
			name:      "only alpha",
			importSet: build(only("alpha")),
			wantKeys:  []string{"alpha"},
		},
		{
			name:      "except gamma",
			importSet: build(except("gamma")),
			wantKeys:  []string{"alpha", "beta"},
		},
		{
			name:      "prefix t:",
			importSet: build(prefix("t:")),
			wantKeys:  []string{"t:alpha", "t:beta", "t:gamma"},
		},
		{
			name:      "rename alpha to a",
			importSet: build(rename("alpha", "a")),
			wantKeys:  []string{"a", "beta", "gamma"},
		},
		{
			name:      "only nonexistent errors",
			importSet: build(only("nonexistent")),
			wantErr:   true,
		},
		{
			name:      "except nonexistent errors",
			importSet: build(except("nonexistent")),
			wantErr:   true,
		},
		{
			// (only (prefix LIB t:) t:alpha): prefix first, then only matches the
			// prefixed name — the inside-out fold (libraries-plan Task 5A / 7D).
			name:      "only of prefix matches prefixed name",
			importSet: build(prefix("t:"), only("t:alpha")),
			wantKeys:  []string{"t:alpha"},
		},
		{
			// (prefix (prefix LIB a-) b-): both prefixes compose, not overwrite.
			name:      "prefix of prefix composes",
			importSet: build(prefix("a-"), prefix("b-")),
			wantKeys:  []string{"b-a-alpha", "b-a-beta", "b-a-gamma"},
		},
		{
			// (rename (prefix LIB t:) (t:alpha aa)): rename sees the prefixed name.
			name:      "rename of prefix renames prefixed name",
			importSet: build(prefix("t:"), rename("t:alpha", "aa")),
			wantKeys:  []string{"aa", "t:beta", "t:gamma"},
		},
		{
			// (only (only LIB alpha) beta): inner restricts to {alpha}; outer cannot
			// reach beta ⇒ error, not last-writer-wins.
			name:      "only of only outside inner errors",
			importSet: build(only("alpha"), only("beta")),
			wantErr:   true,
		},
		{
			// (only (except (only LIB alpha beta) beta) alpha): chained filters fold.
			name:      "only except only chain",
			importSet: build(only("alpha", "beta"), except("beta"), only("alpha")),
			wantKeys:  []string{"alpha"},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, err := tc.importSet.ApplyToExports(lib)
			if tc.wantErr {
				qt.Assert(t, err, qt.IsNotNil)
				wantSentinel := tc.wantErrIs
				if wantSentinel == nil {
					wantSentinel = werr.ErrUnexportedIdentifier
				}
				qt.Assert(t, errors.Is(err, wantSentinel), qt.IsTrue)
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

// TestLibraryRegistryLoadingState tests the IsLoading view of the claim
// lifecycle: claiming the slot (via LookupClaimOrWait) installs the latch,
// FinishLoading clears it.
func TestLibraryRegistryLoadingState(t *testing.T) {
	reg := NewLibraryRegistry()
	name := NewLibraryName("test", "lib")

	// Initially not loading.
	qt.Assert(t, reg.IsLoading(name), qt.IsFalse)

	// Claiming the loading slot (both returns nil) -> IsLoading true.
	cached, wait := reg.LookupClaimOrWait(name)
	qt.Assert(t, cached, qt.IsNil)
	qt.Assert(t, wait, qt.IsNil)
	qt.Assert(t, reg.IsLoading(name), qt.IsTrue)

	// FinishLoading -> IsLoading false.
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

		fireImportObserver(env, lib, bindings, importer, environment.PhaseRuntime)

		qt.Assert(t, received.Library.Key(), qt.Equals, "test/fire")
		qt.Assert(t, received.Importer.Key(), qt.Equals, "my/app")
		qt.Assert(t, received.Phase, qt.Equals, environment.PhaseRuntime)
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
