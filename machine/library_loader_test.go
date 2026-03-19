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
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/machine"

	qt "github.com/frankban/quicktest"
)

// TestLibraryLoaderNotFound tests that loading a nonexistent library produces
// an appropriate error.
func TestLibraryLoaderNotFound(t *testing.T) {
	c := qt.New(t)

	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
	c.Assert(err, qt.IsNil)
	env.TopLevelEnv().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)

	registry := machine.NewLibraryRegistry()
	registry.SetSearchPaths([]string{"/nonexistent/dir"})
	env.SetLibraryRegistry(registry)

	name := machine.NewLibraryName("no", "such", "library")
	_, err = machine.LoadLibrary(context.Background(), name, env)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "not found")
}

// TestLibraryLoaderNoRegistry tests that LoadLibrary fails gracefully when
// no library registry is configured on the environment.
func TestLibraryLoaderNoRegistry(t *testing.T) {
	c := qt.New(t)

	env := environment.NewTopLevelEnvironment().Runtime()
	// Intentionally NOT setting a library registry

	name := machine.NewLibraryName("scheme", "base")
	_, err := machine.LoadLibrary(context.Background(), name, env)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "no library registry")
}

// TestLibraryLoaderMalformedFile tests loading a file that does not contain
// a valid define-library form.
func TestLibraryLoaderMalformedFile(t *testing.T) {
	c := qt.New(t)

	// Create a temp directory with a malformed library file
	tmpDir := t.TempDir()
	libDir := filepath.Join(tmpDir, "bad")
	err := os.MkdirAll(libDir, 0o755)
	c.Assert(err, qt.IsNil)

	// Write a file that is NOT a define-library form
	malformedPath := filepath.Join(libDir, "lib.sld")
	err = os.WriteFile(malformedPath, []byte(`(not-a-library "hello")`), 0o644)
	c.Assert(err, qt.IsNil)

	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
	c.Assert(err, qt.IsNil)
	env.TopLevelEnv().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)

	registry := machine.NewLibraryRegistry()
	registry.SetSearchPaths([]string{tmpDir})
	env.SetLibraryRegistry(registry)

	name := machine.NewLibraryName("bad", "lib")
	_, err = machine.LoadLibrary(context.Background(), name, env)
	c.Assert(err, qt.IsNotNil)
}

// TestLibraryLoaderEmptyFile tests loading an empty library file.
func TestLibraryLoaderEmptyFile(t *testing.T) {
	c := qt.New(t)

	tmpDir := t.TempDir()
	libDir := filepath.Join(tmpDir, "empty")
	err := os.MkdirAll(libDir, 0o755)
	c.Assert(err, qt.IsNil)

	emptyPath := filepath.Join(libDir, "lib.sld")
	err = os.WriteFile(emptyPath, []byte(""), 0o644)
	c.Assert(err, qt.IsNil)

	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
	c.Assert(err, qt.IsNil)
	env.TopLevelEnv().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)

	registry := machine.NewLibraryRegistry()
	registry.SetSearchPaths([]string{tmpDir})
	env.SetLibraryRegistry(registry)

	name := machine.NewLibraryName("empty", "lib")
	_, err = machine.LoadLibrary(context.Background(), name, env)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "empty")
}

// TestLibraryLoaderNameMismatch tests that the loader rejects a library whose
// internal name does not match the expected name.
func TestLibraryLoaderNameMismatch(t *testing.T) {
	c := qt.New(t)

	tmpDir := t.TempDir()
	libDir := filepath.Join(tmpDir, "wrong")
	err := os.MkdirAll(libDir, 0o755)
	c.Assert(err, qt.IsNil)

	// Write a library file whose name is (wrong actual-name) but we'll
	// try to load it as (wrong lib)
	libContent := `(define-library (wrong actual-name)
	  (export x)
	  (begin (define x 1)))`
	libPath := filepath.Join(libDir, "lib.sld")
	err = os.WriteFile(libPath, []byte(libContent), 0o644)
	c.Assert(err, qt.IsNil)

	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
	c.Assert(err, qt.IsNil)
	env.TopLevelEnv().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)

	registry := machine.NewLibraryRegistry()
	registry.SetSearchPaths([]string{tmpDir})
	env.SetLibraryRegistry(registry)

	name := machine.NewLibraryName("wrong", "lib")
	_, err = machine.LoadLibrary(context.Background(), name, env)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "mismatch")
}

// TestLibraryLoaderCachedReturn tests that a second load of the same library
// returns the cached version without re-reading the file.
func TestLibraryLoaderCachedReturn(t *testing.T) {
	c := qt.New(t)
	env := setupLibraryTest(t)

	name := machine.NewLibraryName("test", "simple")

	lib1, err := machine.LoadLibrary(context.Background(), name, env)
	c.Assert(err, qt.IsNil)
	c.Assert(lib1, qt.IsNotNil)

	lib2, err := machine.LoadLibrary(context.Background(), name, env)
	c.Assert(err, qt.IsNil)

	// Same pointer — returned from cache, not recompiled
	c.Assert(lib1, qt.Equals, lib2)
}
