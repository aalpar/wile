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
	"os"
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/werr"
)

func TestParseSummary(t *testing.T) {
	ctx := context.Background()

	tcs := []struct {
		name        string
		input       string
		libName     LibraryName
		wantExports []string
		wantDesc    string
		wantErr     error
	}{
		{
			name: "simple library with exports and description",
			input: `(define-library (example lib)
				(description "An example library")
				(export foo bar baz))`,
			libName:     NewLibraryName("example", "lib"),
			wantExports: []string{"foo", "bar", "baz"},
			wantDesc:    "An example library",
		},
		{
			name: "rename export extracts external name",
			input: `(define-library (my lib)
				(export alpha (rename internal-name external-name)))`,
			libName:     NewLibraryName("my", "lib"),
			wantExports: []string{"alpha", "external-name"},
		},
		{
			name: "no description",
			input: `(define-library (nodesc)
				(export x y))`,
			libName:     NewLibraryName("nodesc"),
			wantExports: []string{"x", "y"},
			wantDesc:    "",
		},
		{
			name:        "empty library no declarations",
			input:       `(define-library (empty))`,
			libName:     NewLibraryName("empty"),
			wantExports: nil,
			wantDesc:    "",
		},
		{
			name: "library keyword alternative",
			input: `(library (alt)
				(export a b))`,
			libName:     NewLibraryName("alt"),
			wantExports: []string{"a", "b"},
		},
		{
			name: "multiple export clauses",
			input: `(define-library (multi)
				(export a b)
				(export c d))`,
			libName:     NewLibraryName("multi"),
			wantExports: []string{"a", "b", "c", "d"},
		},
		{
			name:    "not a library form",
			input:   `(define (f x) x)`,
			libName: NewLibraryName("irrelevant"),
			wantErr: werr.ErrLibraryFormMalformed,
		},
		{
			name:    "empty file",
			input:   ``,
			libName: NewLibraryName("irrelevant"),
			wantErr: werr.ErrLibraryFormMalformed,
		},
		{
			name:    "atom not list",
			input:   `42`,
			libName: NewLibraryName("irrelevant"),
			wantErr: werr.ErrLibraryFormMalformed,
		},
		{
			name:    "no library name",
			input:   `(define-library)`,
			libName: NewLibraryName("any"),
			wantErr: werr.ErrLibraryFormMalformed,
		},
		{
			name:    "library name mismatch",
			input:   `(define-library (actual name) (export x))`,
			libName: NewLibraryName("wrong", "name"),
			wantErr: werr.ErrLibraryFormMalformed,
		},
		{
			name: "unknown declarations skipped",
			input: `(define-library (skip)
				(import (scheme base))
				(begin (define x 1))
				(export x))`,
			libName:     NewLibraryName("skip"),
			wantExports: []string{"x"},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)

			summary, err := ParseLibrarySummary(ctx, strings.NewReader(tc.input), "test.sld", tc.libName)

			if tc.wantErr != nil {
				c.Assert(err, qt.IsNotNil)
				c.Assert(errors.Is(err, tc.wantErr), qt.IsTrue, qt.Commentf("got: %v", err))
				return
			}

			c.Assert(err, qt.IsNil)
			c.Assert(summary.Name, qt.DeepEquals, tc.libName)
			c.Assert(summary.Description, qt.Equals, tc.wantDesc)
			c.Assert(summary.Exports, qt.DeepEquals, tc.wantExports)
			c.Assert(summary.SourceFile, qt.Equals, "test.sld")
		})
	}
}

func TestParseSummaryRealSldFile(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	f, err := os.Open("../../stdlib/lib/scheme/char.sld")
	c.Assert(err, qt.IsNil)
	defer f.Close()

	summary, err := ParseLibrarySummary(ctx, f, "scheme/char.sld", NewLibraryName("scheme", "char"))
	c.Assert(err, qt.IsNil)

	// Verify known exports are present.
	exports := make(map[string]bool, len(summary.Exports))
	for _, e := range summary.Exports {
		exports[e] = true
	}

	c.Assert(exports["char-upcase"], qt.IsTrue, qt.Commentf("expected char-upcase in exports"))
	c.Assert(exports["string-downcase"], qt.IsTrue, qt.Commentf("expected string-downcase in exports"))

	// Description should be non-empty.
	c.Assert(summary.Description != "", qt.IsTrue, qt.Commentf("expected non-empty description"))
}

func TestLibraryExportIndexLookup(t *testing.T) {
	c := qt.New(t)

	summary := &LibrarySummary{
		Name:        NewLibraryName("scheme", "base"),
		Description: "Base library",
		Exports:     []string{"define", "lambda"},
		SourceFile:  "scheme/base.sld",
	}
	idx := NewLibraryExportIndexFromEntries(map[string]*LibrarySummary{
		summary.Name.Key(): summary,
	})

	got := idx.Lookup(NewLibraryName("scheme", "base"))
	c.Assert(got, qt.Equals, summary)

	c.Assert(idx.Lookup(NewLibraryName("nonexistent")), qt.IsNil)
}

func TestLibraryExportIndexLookupNilReceiver(t *testing.T) {
	c := qt.New(t)
	var idx *LibraryExportIndex
	c.Assert(idx.Lookup(NewLibraryName("any")), qt.IsNil)
}

func TestLibraryExportIndexEntries(t *testing.T) {
	c := qt.New(t)

	idx := NewLibraryExportIndexFromEntries(map[string]*LibrarySummary{
		"b": {Name: NewLibraryName("b")},
		"a": {Name: NewLibraryName("a")},
	})

	entries := idx.Entries()
	c.Assert(len(entries), qt.Equals, 2)
	// Verify sorted by key.
	c.Assert(entries[0].Name.Key(), qt.Equals, "a")
	c.Assert(entries[1].Name.Key(), qt.Equals, "b")
}

func TestLibraryExportIndexEntriesNilReceiver(t *testing.T) {
	c := qt.New(t)
	var idx *LibraryExportIndex
	c.Assert(idx.Entries(), qt.IsNil)
}

func TestBuildExportIndex(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"scheme/base.sld": &fstest.MapFile{
			Data: []byte(`(define-library (scheme base)
				(description "Base library")
				(export define lambda if))`),
		},
		"my/utils.sld": &fstest.MapFile{
			Data: []byte(`(define-library (my utils)
				(export helper))`),
		},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"."})
	ns.SetLibraryRegistry(reg)

	resolver := NewFSFileResolver(fsys, env)

	idx, err := BuildExportIndex(ctx, resolver, reg)
	c.Assert(err, qt.IsNil)

	base := idx.Lookup(NewLibraryName("scheme", "base"))
	c.Assert(base, qt.IsNotNil)
	c.Assert(base.Exports, qt.DeepEquals, []string{"define", "lambda", "if"})
	c.Assert(base.Description, qt.Equals, "Base library")

	utils := idx.Lookup(NewLibraryName("my", "utils"))
	c.Assert(utils, qt.IsNotNil)
	c.Assert(utils.Exports, qt.DeepEquals, []string{"helper"})
}

func TestBuildExportIndex_SkipsLoadedLibraries(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"scheme/base.sld": &fstest.MapFile{
			Data: []byte(`(define-library (scheme base)
				(export define lambda))`),
		},
		"my/utils.sld": &fstest.MapFile{
			Data: []byte(`(define-library (my utils)
				(export helper))`),
		},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"."})
	ns.SetLibraryRegistry(reg)

	// Register (scheme base) as already loaded.
	alreadyLoaded := NewCompiledLibrary(NewLibraryName("scheme", "base"), env)
	c.Assert(reg.Register(alreadyLoaded), qt.IsNil)

	resolver := NewFSFileResolver(fsys, env)

	idx, err := BuildExportIndex(ctx, resolver, reg)
	c.Assert(err, qt.IsNil)

	// (scheme base) should be excluded — it's already loaded.
	c.Assert(idx.Lookup(NewLibraryName("scheme", "base")), qt.IsNil)

	// (my utils) should be indexed.
	utils := idx.Lookup(NewLibraryName("my", "utils"))
	c.Assert(utils, qt.IsNotNil)
	c.Assert(utils.Exports, qt.DeepEquals, []string{"helper"})
}

func TestBuildExportIndex_MalformedFilesReturnError(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"good/lib.sld": &fstest.MapFile{
			Data: []byte(`(define-library (good lib)
				(export working))`),
		},
		"bad/lib.sld": &fstest.MapFile{
			Data: []byte(`this is not valid library syntax`),
		},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"."})
	ns.SetLibraryRegistry(reg)

	resolver := NewFSFileResolver(fsys, env)

	idx, err := BuildExportIndex(ctx, resolver, reg)
	// Parse errors are surfaced alongside partial results.
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrLibraryFormMalformed), qt.IsTrue)

	// Good library should still be indexed (partial results).
	good := idx.Lookup(NewLibraryName("good", "lib"))
	c.Assert(good, qt.IsNotNil)
	c.Assert(good.Exports, qt.DeepEquals, []string{"working"})

	// Bad library is not indexed.
	c.Assert(idx.Lookup(NewLibraryName("bad", "lib")), qt.IsNil)
}

func TestBuildExportIndex_NoEnumerator(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// EmbedFileResolver does not implement FileEnumerator.
	resolver := NewEmbedFileResolver(fstest.MapFS{})

	idx, err := BuildExportIndex(ctx, resolver, nil)
	c.Assert(err, qt.IsNil)
	c.Assert(idx.Entries(), qt.HasLen, 0)
}
