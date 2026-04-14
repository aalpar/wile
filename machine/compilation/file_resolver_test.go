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

// ResolveAndOpen tests live in resolver/resolver_test.go.
// This file tests enumeration → FilePathToLibraryName integration
// and compat-layer interface compliance.

import (
	"os"
	"path/filepath"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine/compilation/resolver"
)

// realDir normalizes a temp directory path to account for macOS symlinks
// (/tmp -> /private/tmp). Without this, paths from t.TempDir() and
// filepath.Abs() inside resolvers won't match.
func realDir(t *testing.T, dir string) string {
	t.Helper()
	resolved, err := filepath.EvalSymlinks(dir)
	qt.Assert(t, err, qt.IsNil)
	return resolved
}

// --- FSFileResolver EnumerateFiles ---

func TestFSFileResolverEnumerateFiles(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"scheme/base.sld":  &fstest.MapFile{Data: []byte("(define-library (scheme base))")},
		"scheme/write.sld": &fstest.MapFile{Data: []byte("(define-library (scheme write))")},
		"chibi/test.scm":   &fstest.MapFile{Data: []byte("(define-library (chibi test))")},
		".hidden/lib.sld":  &fstest.MapFile{Data: []byte("skip")},
		"readme.txt":       &fstest.MapFile{Data: []byte("not a library")},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"."})
	ns.SetLibraryRegistry(reg)

	res := NewFSFileResolver(fsys, env)

	// Verify FSFileResolver satisfies FileEnumerator via interface variable.
	var fr FileResolver = res
	fileEnum, ok := fr.(resolver.FileEnumerator)
	c.Assert(ok, qt.IsTrue)

	files, err := fileEnum.EnumerateFiles()
	c.Assert(err, qt.IsNil)

	var keys []string
	for _, path := range files {
		name, nameErr := FilePathToLibraryName(path)
		c.Assert(nameErr, qt.IsNil)
		keys = append(keys, name.Key())
	}
	c.Assert(keys, qt.DeepEquals, []string{"chibi/test", "scheme/base", "scheme/write"})
}

func TestFSFileResolverEnumerateWithSearchPaths(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"lib/scheme/base.sld":  &fstest.MapFile{Data: []byte("")},
		"lib/scheme/write.sld": &fstest.MapFile{Data: []byte("")},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"lib"})
	ns.SetLibraryRegistry(reg)

	res := NewFSFileResolver(fsys, env)

	files, err := res.EnumerateFiles()
	c.Assert(err, qt.IsNil)

	var keys []string
	for _, path := range files {
		name, nameErr := FilePathToLibraryName(path)
		c.Assert(nameErr, qt.IsNil)
		keys = append(keys, name.Key())
	}
	c.Assert(keys, qt.DeepEquals, []string{"scheme/base", "scheme/write"})
}

func TestFSFileResolverEnumerateSldAndScm(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"scheme/base.sld": &fstest.MapFile{Data: []byte("")},
		"scheme/base.scm": &fstest.MapFile{Data: []byte("")},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"."})
	ns.SetLibraryRegistry(reg)

	res := NewFSFileResolver(fsys, env)

	files, err := res.EnumerateFiles()
	c.Assert(err, qt.IsNil)
	// EnumerateFiles returns both .sld and .scm; dedup is caller's job.
	// Convert to library names and deduplicate.
	seen := make(map[string]bool)
	var keys []string
	for _, path := range files {
		name, nameErr := FilePathToLibraryName(path)
		c.Assert(nameErr, qt.IsNil)
		key := name.Key()
		if !seen[key] {
			seen[key] = true
			keys = append(keys, key)
		}
	}
	c.Assert(len(keys), qt.Equals, 1)
	c.Assert(keys[0], qt.Equals, "scheme/base")
}

// --- OSFileResolver EnumerateFiles ---

func TestOSFileResolverEnumerateFiles(t *testing.T) {
	c := qt.New(t)

	dir := realDir(t, t.TempDir())

	libDir := filepath.Join(dir, "libs")
	c.Assert(os.MkdirAll(filepath.Join(libDir, "scheme"), 0o755), qt.IsNil)
	c.Assert(os.WriteFile(filepath.Join(libDir, "scheme", "base.sld"), []byte(""), 0o644), qt.IsNil)
	c.Assert(os.WriteFile(filepath.Join(libDir, "scheme", "write.sld"), []byte(""), 0o644), qt.IsNil)

	c.Assert(os.MkdirAll(filepath.Join(libDir, ".hidden"), 0o755), qt.IsNil)
	c.Assert(os.WriteFile(filepath.Join(libDir, ".hidden", "skip.sld"), []byte(""), 0o644), qt.IsNil)

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{libDir})
	ns.SetLibraryRegistry(reg)

	t.Setenv(SchemeIncludePathEnv, "")

	// Chdir to an empty temp dir so CWD fallback doesn't find project libraries.
	t.Chdir(t.TempDir())

	res := NewOSFileResolver(env)
	files, err := res.EnumerateFiles()
	c.Assert(err, qt.IsNil)

	var keys []string
	for _, path := range files {
		name, nameErr := FilePathToLibraryName(path)
		c.Assert(nameErr, qt.IsNil)
		keys = append(keys, name.Key())
	}
	c.Assert(keys, qt.DeepEquals, []string{"scheme/base", "scheme/write"})
}

func TestOSFileResolverEnumerateEmptySearchPaths(t *testing.T) {
	c := qt.New(t)

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"/nonexistent/path"})
	ns.SetLibraryRegistry(reg)

	t.Setenv(SchemeIncludePathEnv, "")

	// Chdir to an empty temp dir so CWD fallback doesn't find project libraries.
	t.Chdir(t.TempDir())

	res := NewOSFileResolver(env)
	files, err := res.EnumerateFiles()
	c.Assert(err, qt.IsNil)
	c.Assert(len(files), qt.Equals, 0)
}

// --- ChainFileResolver EnumerateFiles ---

func TestChainFileResolverEnumerateFiles(t *testing.T) {
	c := qt.New(t)

	fs1 := fstest.MapFS{
		"scheme/base.sld": &fstest.MapFile{Data: []byte("")},
		"my/lib.sld":      &fstest.MapFile{Data: []byte("")},
	}
	fs2 := fstest.MapFS{
		"scheme/base.sld":  &fstest.MapFile{Data: []byte("")},
		"scheme/write.sld": &fstest.MapFile{Data: []byte("")},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"."})
	ns.SetLibraryRegistry(reg)

	chain := NewChainFileResolver([]FileResolver{
		NewFSFileResolver(fs1, env),
		NewFSFileResolver(fs2, env),
	})

	files, err := chain.EnumerateFiles()
	c.Assert(err, qt.IsNil)

	seen := make(map[string]bool)
	var keys []string
	for _, path := range files {
		name, nameErr := FilePathToLibraryName(path)
		c.Assert(nameErr, qt.IsNil)
		key := name.Key()
		if !seen[key] {
			seen[key] = true
			keys = append(keys, key)
		}
	}
	c.Assert(keys, qt.DeepEquals, []string{"my/lib", "scheme/base", "scheme/write"})
}

// --- Interface compliance ---

func TestFileResolverInterfaceCompliance(t *testing.T) {
	// Compile-time check that all types satisfy the interface.
	var _ FileResolver = (*OSFileResolver)(nil)
	var _ FileResolver = (*EmbedFileResolver)(nil)
	var _ FileResolver = (*FSFileResolver)(nil)
	var _ FileResolver = (*ChainFileResolver)(nil)

	// Compile-time check that enumerator types satisfy FileEnumerator.
	var _ resolver.FileEnumerator = (*FSFileResolver)(nil)
	var _ resolver.FileEnumerator = (*OSFileResolver)(nil)
	var _ resolver.FileEnumerator = (*ChainFileResolver)(nil)
}
