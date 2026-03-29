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

package machine

import (
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
)

func TestDiscoverAvailableLibraries(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"scheme/base.sld": &fstest.MapFile{Data: []byte("")},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"."})
	ns.SetLibraryRegistry(reg)

	syntheticLib := NewCompiledLibrary(NewLibraryName("wile", "io"), env)
	c.Assert(reg.Register(syntheticLib), qt.IsNil)

	resolver := NewFSFileResolver(fsys, env)

	libs, err := DiscoverAvailableLibraries(resolver, reg)
	c.Assert(err, qt.IsNil)

	keys := make([]string, len(libs))
	for i, lib := range libs {
		keys[i] = lib.Key()
	}
	c.Assert(keys, qt.DeepEquals, []string{"scheme/base", "wile/io"})
}

func TestDiscoverAvailableLibrariesNoEnumerator(t *testing.T) {
	c := qt.New(t)

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	ns.SetLibraryRegistry(reg)

	syntheticLib := NewCompiledLibrary(NewLibraryName("wile", "io"), env)
	c.Assert(reg.Register(syntheticLib), qt.IsNil)

	resolver := NewEmbedFileResolver(fstest.MapFS{})

	libs, err := DiscoverAvailableLibraries(resolver, reg)
	c.Assert(err, qt.IsNil)
	c.Assert(len(libs), qt.Equals, 1)
	c.Assert(libs[0].Key(), qt.Equals, "wile/io")
}

func TestDiscoverAvailableLibrariesNilRegistry(t *testing.T) {
	c := qt.New(t)

	resolver := NewEmbedFileResolver(fstest.MapFS{})

	libs, err := DiscoverAvailableLibraries(resolver, nil)
	c.Assert(err, qt.IsNil)
	c.Assert(len(libs), qt.Equals, 0)
}

func TestDiscoverAvailableLibrariesNilResolver(t *testing.T) {
	c := qt.New(t)

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	ns.SetLibraryRegistry(reg)

	syntheticLib := NewCompiledLibrary(NewLibraryName("wile", "io"), env)
	c.Assert(reg.Register(syntheticLib), qt.IsNil)

	libs, err := DiscoverAvailableLibraries(nil, reg)
	c.Assert(err, qt.IsNil)
	c.Assert(len(libs), qt.Equals, 1)
	c.Assert(libs[0].Key(), qt.Equals, "wile/io")
}
