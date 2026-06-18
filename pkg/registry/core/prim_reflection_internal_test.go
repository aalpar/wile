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

package core

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/registry"
)

// libNameParts holds a library name as parts plus a description, used to
// build compilation fixtures in the adapter tests below.
type libNameParts struct {
	parts []string
	desc  string
}

func TestLibraryRegistrySearcherAdapter(t *testing.T) {
	tcs := []struct {
		name    string
		entries []libNameParts
		want    []registry.LibraryDoc
	}{
		{
			name: "empty registry",
			want: []registry.LibraryDoc{},
		},
		{
			name: "multiple libraries sorted by name key",
			entries: []libNameParts{
				{parts: []string{"wile", "math"}, desc: "Math library"},
				{parts: []string{"srfi", "1"}, desc: "List library"},
			},
			want: []registry.LibraryDoc{
				{Name: "(srfi 1)", Description: "List library"},
				{Name: "(wile math)", Description: "Math library"},
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			libReg := compilation.NewLibraryRegistry()
			for _, e := range tc.entries {
				name := compilation.NewLibraryName(e.parts...)
				lib := compilation.NewCompiledLibrary(name, environment.NewNamespace().Runtime())
				lib.Description = e.desc
				err := libReg.Register(lib)
				c.Assert(err, qt.IsNil)
			}

			adapter := libraryRegistrySearcher{reg: libReg}
			c.Assert(adapter.AllLibraries(), qt.DeepEquals, tc.want)
		})
	}
}

func TestLibraryExportIndexSearcherAdapter(t *testing.T) {
	tcs := []struct {
		name    string
		entries map[string]*compilation.LibrarySummary
		want    []registry.LibraryExportDoc
	}{
		{
			name: "empty index",
			want: []registry.LibraryExportDoc{},
		},
		{
			name: "single library with exports",
			entries: map[string]*compilation.LibrarySummary{
				"srfi/1": {
					Name:        compilation.NewLibraryName("srfi", "1"),
					Description: "List library",
					Exports:     []string{"fold", "partition"},
				},
			},
			want: []registry.LibraryExportDoc{
				{
					Name:        "(srfi 1)",
					Description: "List library",
					Exports:     []string{"fold", "partition"},
				},
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			idx := compilation.NewLibraryExportIndexFromEntries(tc.entries)
			adapter := libraryExportIndexSearcher{idx: idx}
			c.Assert(adapter.AllLibraryExports(), qt.DeepEquals, tc.want)
		})
	}
}
