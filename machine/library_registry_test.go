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

package machine_test

import (
	"fmt"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"

	qt "github.com/frankban/quicktest"
)

// TestLibraryRegistryDuplicateRegister tests that registering a library with the
// same name twice returns an error.
func TestLibraryRegistryDuplicateRegister(t *testing.T) {
	c := qt.New(t)

	registry := machine.NewLibraryRegistry()
	env := environment.NewNamespace().Runtime()
	name := machine.NewLibraryName("test", "duplib")
	lib := machine.NewCompiledLibrary(name, env)

	err := registry.Register(lib)
	c.Assert(err, qt.IsNil)

	err = registry.Register(lib)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "already registered")
}

// TestLibraryRegistryLoadingCycle tests the IsLoading/StartLoading/FinishLoading
// cycle detection mechanism.
func TestLibraryRegistryLoadingCycle(t *testing.T) {
	c := qt.New(t)

	registry := machine.NewLibraryRegistry()
	name := machine.NewLibraryName("test", "loading")

	c.Assert(registry.IsLoading(name), qt.IsFalse)

	registry.StartLoading(name)
	c.Assert(registry.IsLoading(name), qt.IsTrue)

	registry.FinishLoading(name)
	c.Assert(registry.IsLoading(name), qt.IsFalse)
}

// TestLibraryRegistryFindLibraryFile tests that FindLibraryFile returns an error
// when no matching file exists in the search paths.
func TestLibraryRegistryFindLibraryFile(t *testing.T) {
	c := qt.New(t)

	registry := machine.NewLibraryRegistry()
	registry.SetSearchPaths([]string{"/nonexistent/path"})

	name := machine.NewLibraryName("no", "such", "lib")
	_, err := registry.FindLibraryFile(name)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "not found")
}

// TestLibraryRegistryImportObserver tests the import observer mechanism.
func TestLibraryRegistryImportObserver(t *testing.T) {
	c := qt.New(t)

	registry := machine.NewLibraryRegistry()

	// No observer initially
	c.Assert(registry.ImportObserver(), qt.IsNil)

	// Set an observer
	var called bool
	obs := func(event machine.LibraryImportEvent) {
		called = true
	}
	registry.SetImportObserver(obs)
	c.Assert(registry.ImportObserver(), qt.IsNotNil)

	// Remove the observer
	registry.SetImportObserver(nil)
	c.Assert(registry.ImportObserver(), qt.IsNil)
	c.Assert(called, qt.IsFalse)
}

// TestLibraryNameSinglePart tests LibraryName with a single part.
func TestLibraryNameSinglePart(t *testing.T) {
	c := qt.New(t)

	name := machine.NewLibraryName("solo")
	c.Assert(name.String(), qt.Equals, "solo")
	c.Assert(name.SchemeString(), qt.Equals, "(solo)")
	c.Assert(name.Key(), qt.Equals, "solo")
}

// TestCompiledLibraryGetInternalNameNotExported tests that GetInternalName
// returns empty string for a name that is not exported.
func TestCompiledLibraryGetInternalNameNotExported(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	name := machine.NewLibraryName("test", "lib")
	lib := machine.NewCompiledLibrary(name, env)

	c.Assert(lib.GetInternalName("nonexistent"), qt.Equals, "")
}

func TestFilePathToLibraryName(t *testing.T) {
	tests := []struct {
		name    string
		path    string
		wantKey string
		wantErr bool
	}{
		{"sld extension", "scheme/base.sld", "scheme/base", false},
		{"scm extension", "chibi/test.scm", "chibi/test", false},
		{"nested path", "wile/algebra/rewrite.sld", "wile/algebra/rewrite", false},
		{"single component", "base.sld", "base", false},
		{"no extension", "scheme/base", "", true},
		{"wrong extension", "scheme/base.txt", "", true},
		{"empty path", "", "", true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result, err := machine.FilePathToLibraryName(tt.path)
			if tt.wantErr {
				c.Assert(err, qt.IsNotNil)
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(result.Key(), qt.Equals, tt.wantKey)
		})
	}
}

func TestLibraryNameToSchemeValue(t *testing.T) {
	tests := []struct {
		name   string
		parts  []string
		expect string
	}{
		{"symbol parts", []string{"scheme", "base"}, "(scheme base)"},
		{"integer part", []string{"srfi", "1"}, "(srfi 1)"},
		{"single part", []string{"base"}, "(base)"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			ln := machine.NewLibraryName(tt.parts...)
			result := ln.ToSchemeValue()
			c.Assert(fmt.Sprint(result), qt.Equals, tt.expect)
		})
	}
}

func TestLibraryRegistryAllNames(t *testing.T) {
	c := qt.New(t)
	reg := machine.NewLibraryRegistry()
	env := environment.NewNamespace().Runtime()

	lib1 := machine.NewCompiledLibrary(machine.NewLibraryName("scheme", "base"), env)
	c.Assert(reg.Register(lib1), qt.IsNil)
	lib2 := machine.NewCompiledLibrary(machine.NewLibraryName("wile", "io"), env)
	c.Assert(reg.Register(lib2), qt.IsNil)

	names := reg.AllNames()
	c.Assert(len(names), qt.Equals, 2)
	c.Assert(names[0].Key(), qt.Equals, "scheme/base")
	c.Assert(names[1].Key(), qt.Equals, "wile/io")
}

func TestLibraryRegistryAllNamesEmpty(t *testing.T) {
	c := qt.New(t)
	reg := machine.NewLibraryRegistry()
	names := reg.AllNames()
	c.Assert(len(names), qt.Equals, 0)
}
