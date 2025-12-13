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

package environment

import (
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestOnlyExportDirective_Next(t *testing.T) {
	sym1 := *values.NewSymbol("export1")
	sym2 := *values.NewSymbol("export2")

	d2 := &OnlyExportDirective{next: nil, only: sym2}
	d1 := &OnlyExportDirective{next: d2, only: sym1}

	qt.Assert(t, d1.Next(), qt.Equals, d2)
	qt.Assert(t, d2.Next(), qt.IsNil)
}

func TestRenameExportDirective_Next(t *testing.T) {
	from := *values.NewSymbol("old")
	to := *values.NewSymbol("new")

	d2 := &RenameExportDirective{next: nil, from: from, to: to}
	d1 := &RenameExportDirective{next: d2, from: from, to: to}

	qt.Assert(t, d1.Next(), qt.Equals, d2)
	qt.Assert(t, d2.Next(), qt.IsNil)
}

func TestLibraryImportDirective_Next(t *testing.T) {
	lib1 := *values.NewSymbol("lib1")
	lib2 := *values.NewSymbol("lib2")

	d2 := &LibraryImportDirective{next: nil, library: lib2}
	d1 := &LibraryImportDirective{next: d2, library: lib1}

	qt.Assert(t, d1.Next(), qt.Equals, d2)
	qt.Assert(t, d2.Next(), qt.IsNil)
}

func TestOnlyImportDirective_Next(t *testing.T) {
	sym1 := *values.NewSymbol("import1")
	sym2 := *values.NewSymbol("import2")

	d2 := &OnlyImportDirective{next: nil, only: sym2}
	d1 := &OnlyImportDirective{next: d2, only: sym1}

	qt.Assert(t, d1.Next(), qt.Equals, d2)
	qt.Assert(t, d2.Next(), qt.IsNil)
}

func TestExceptImportDirective_Next(t *testing.T) {
	sym1 := *values.NewSymbol("except1")
	sym2 := *values.NewSymbol("except2")

	d2 := &ExceptImportDirective{next: nil, except: sym2}
	d1 := &ExceptImportDirective{next: d2, except: sym1}

	qt.Assert(t, d1.Next(), qt.Equals, d2)
	qt.Assert(t, d2.Next(), qt.IsNil)
}

func TestPrefixImportDirective_Next(t *testing.T) {
	prefix1 := *values.NewSymbol("prefix1")
	prefix2 := *values.NewSymbol("prefix2")

	d2 := &PrefixImportDirective{next: nil, prefix: prefix2}
	d1 := &PrefixImportDirective{next: d2, prefix: prefix1}

	qt.Assert(t, d1.Next(), qt.Equals, d2)
	qt.Assert(t, d2.Next(), qt.IsNil)
}

func TestRenameImportDirective_Next(t *testing.T) {
	from := *values.NewSymbol("old")
	to := *values.NewSymbol("new")

	d2 := &RenameImportDirective{next: nil, from: from, to: to}
	d1 := &RenameImportDirective{next: d2, from: from, to: to}

	qt.Assert(t, d1.Next(), qt.Equals, d2)
	qt.Assert(t, d2.Next(), qt.IsNil)
}

func TestNewExportSet_Valid(t *testing.T) {
	sym1 := *values.NewSymbol("export1")
	sym2 := *values.NewSymbol("export2")

	d2 := &OnlyExportDirective{next: nil, only: sym2}
	d1 := &RenameExportDirective{next: d2, from: sym1, to: sym2}

	set, err := NewExportSet(d1)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, set, qt.Not(qt.IsNil))
}

func TestNewExportSet_Nil(t *testing.T) {
	set, err := NewExportSet(nil)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, set, qt.Not(qt.IsNil))
}

func TestNewImportSet_Valid(t *testing.T) {
	sym1 := *values.NewSymbol("import1")
	lib := *values.NewSymbol("library")

	d4 := &LibraryImportDirective{next: nil, library: lib}
	d3 := &PrefixImportDirective{next: d4, prefix: sym1}
	d2 := &ExceptImportDirective{next: d3, except: sym1}
	d1 := &OnlyImportDirective{next: d2, only: sym1}

	set, err := NewImportSet(d1)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, set, qt.Not(qt.IsNil))
}

func TestNewImportSet_WithRename(t *testing.T) {
	from := *values.NewSymbol("old")
	to := *values.NewSymbol("new")

	d := &RenameImportDirective{next: nil, from: from, to: to}

	set, err := NewImportSet(d)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, set, qt.Not(qt.IsNil))
}

func TestNewImportSet_Nil(t *testing.T) {
	set, err := NewImportSet(nil)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, set, qt.Not(qt.IsNil))
}
