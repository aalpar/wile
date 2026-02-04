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

	qt "github.com/frankban/quicktest"
)

func TestLibraryName_String(t *testing.T) {
	name := NewLibraryName("scheme", "base")
	qt.Assert(t, name.String(), qt.Equals, "scheme/base")
}

func TestLibraryName_SchemeString(t *testing.T) {
	name := NewLibraryName("scheme", "base")
	qt.Assert(t, name.SchemeString(), qt.Equals, "(scheme base)")
}

func TestLibraryName_Key(t *testing.T) {
	name := NewLibraryName("scheme", "base")
	qt.Assert(t, name.Key(), qt.Equals, "scheme/base")
}

func TestLibraryName_ToFilePath(t *testing.T) {
	name := NewLibraryName("scheme", "base")
	// Should end with .sld
	path := name.ToFilePath()
	qt.Assert(t, path, qt.Contains, "scheme")
	qt.Assert(t, path, qt.Contains, "base")
	qt.Assert(t, path, qt.Contains, ".sld")
}

func TestLibraryName_MultiPart(t *testing.T) {
	name := NewLibraryName("my", "awesome", "library")
	qt.Assert(t, name.String(), qt.Equals, "my/awesome/library")
	qt.Assert(t, name.SchemeString(), qt.Equals, "(my awesome library)")
	qt.Assert(t, name.Key(), qt.Equals, "my/awesome/library")
}

func TestImportSet_NewImportSet(t *testing.T) {
	name := NewLibraryName("scheme", "base")
	importSet := NewImportSet(name)

	qt.Assert(t, importSet.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, importSet.Only, qt.IsNil)
	qt.Assert(t, importSet.Except, qt.IsNil)
	qt.Assert(t, importSet.Prefix, qt.Equals, "")
	qt.Assert(t, importSet.Renames, qt.IsNotNil)
	qt.Assert(t, len(importSet.Renames), qt.Equals, 0)
}

func TestCompiledLibrary_NewCompiledLibrary(t *testing.T) {
	name := NewLibraryName("test", "lib")
	lib := &CompiledLibrary{
		Name:       name,
		Exports:    make(map[string]string),
		SourceFile: "test/lib.sld",
	}

	qt.Assert(t, lib.Name.Key(), qt.Equals, "test/lib")
	qt.Assert(t, lib.SourceFile, qt.Equals, "test/lib.sld")
	qt.Assert(t, lib.Exports, qt.IsNotNil)
}
