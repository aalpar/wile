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
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestParseLibraryNameFromDatum(t *testing.T) {
	// (scheme base)
	libName := values.NewCons(
		values.NewSymbol("scheme"),
		values.NewCons(values.NewSymbol("base"), values.EmptyList),
	)

	result, err := ParseLibraryNameFromDatum(libName)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.Key(), qt.Equals, "scheme/base")
}

func TestParseLibraryNameFromDatum_WithNumbers(t *testing.T) {
	// (srfi 1)
	libName := values.NewCons(
		values.NewSymbol("srfi"),
		values.NewCons(values.NewInteger(1), values.EmptyList),
	)

	result, err := ParseLibraryNameFromDatum(libName)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.Key(), qt.Equals, "srfi/1")
}

func TestParseLibraryNameFromDatum_NotAPair(t *testing.T) {
	_, err := ParseLibraryNameFromDatum(values.NewInteger(42))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "library name must be a list")
}

func TestParseLibraryNameFromDatum_Empty(t *testing.T) {
	_, err := ParseLibraryNameFromDatum(values.EmptyList)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "library name cannot be empty")
}

func TestParseLibraryNameFromDatum_InvalidPart(t *testing.T) {
	// (scheme "invalid")
	libName := values.NewCons(
		values.NewSymbol("scheme"),
		values.NewCons(values.NewString("invalid"), values.EmptyList),
	)

	_, err := ParseLibraryNameFromDatum(libName)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "library name part must be identifier or integer")
}

func TestParseImportSetFromDatum_Simple(t *testing.T) {
	// (scheme base)
	importSet := values.NewCons(
		values.NewSymbol("scheme"),
		values.NewCons(values.NewSymbol("base"), values.EmptyList),
	)

	result, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Only, qt.IsNil)
	qt.Assert(t, result.Except, qt.IsNil)
	qt.Assert(t, result.Prefix, qt.Equals, "")
	qt.Assert(t, result.Renames, qt.HasLen, 0)
}

func TestParseImportSetFromDatum_Only(t *testing.T) {
	// (only (scheme base) car cdr)
	importSet := values.NewCons(
		values.NewSymbol("only"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.NewCons(
				values.NewSymbol("car"),
				values.NewCons(values.NewSymbol("cdr"), values.EmptyList),
			),
		),
	)

	result, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Only, qt.DeepEquals, []string{"car", "cdr"})
}

func TestParseImportSetFromDatum_Except(t *testing.T) {
	// (except (scheme base) car cdr)
	importSet := values.NewCons(
		values.NewSymbol("except"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.NewCons(
				values.NewSymbol("car"),
				values.NewCons(values.NewSymbol("cdr"), values.EmptyList),
			),
		),
	)

	result, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Except, qt.DeepEquals, []string{"car", "cdr"})
}

func TestParseImportSetFromDatum_Prefix(t *testing.T) {
	// (prefix (scheme base) scheme:)
	importSet := values.NewCons(
		values.NewSymbol("prefix"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.NewCons(values.NewSymbol("scheme:"), values.EmptyList),
		),
	)

	result, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Prefix, qt.Equals, "scheme:")
}

func TestParseImportSetFromDatum_Rename(t *testing.T) {
	// (rename (scheme base) (car first) (cdr rest))
	importSet := values.NewCons(
		values.NewSymbol("rename"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.NewCons(
				values.NewCons(
					values.NewSymbol("car"),
					values.NewCons(values.NewSymbol("first"), values.EmptyList),
				),
				values.NewCons(
					values.NewCons(
						values.NewSymbol("cdr"),
						values.NewCons(values.NewSymbol("rest"), values.EmptyList),
					),
					values.EmptyList,
				),
			),
		),
	)

	result, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Renames["car"], qt.Equals, "first")
	qt.Assert(t, result.Renames["cdr"], qt.Equals, "rest")
}

func TestParseImportSetFromDatum_Nested(t *testing.T) {
	// (prefix (only (scheme base) car cdr) scheme:)
	importSet := values.NewCons(
		values.NewSymbol("prefix"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("only"),
				values.NewCons(
					values.NewCons(
						values.NewSymbol("scheme"),
						values.NewCons(values.NewSymbol("base"), values.EmptyList),
					),
					values.NewCons(
						values.NewSymbol("car"),
						values.NewCons(values.NewSymbol("cdr"), values.EmptyList),
					),
				),
			),
			values.NewCons(values.NewSymbol("scheme:"), values.EmptyList),
		),
	)

	result, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Only, qt.DeepEquals, []string{"car", "cdr"})
	qt.Assert(t, result.Prefix, qt.Equals, "scheme:")
}

func TestParseImportSetFromDatum_OnlyEmpty(t *testing.T) {
	// (only (scheme base))
	importSet := values.NewCons(
		values.NewSymbol("only"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.EmptyList,
		),
	)

	result, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Only, qt.IsNil)
}

func TestParseImportSetFromDatum_RenameEmpty(t *testing.T) {
	// (rename (scheme base))
	importSet := values.NewCons(
		values.NewSymbol("rename"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.EmptyList,
		),
	)

	result, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Renames, qt.HasLen, 0)
}

func TestParseImportSetFromDatum_NotAPair(t *testing.T) {
	_, err := ParseImportSetFromDatum(values.NewInteger(42))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "import set must be a list")
}

func TestParseImportSetFromDatum_Only_InvalidFormat(t *testing.T) {
	// (only)
	importSet := values.NewCons(
		values.NewSymbol("only"),
		values.EmptyList,
	)

	_, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "import set must be a list")
}

func TestParseImportSetFromDatum_Prefix_InvalidFormat(t *testing.T) {
	// (prefix (scheme base))
	importSet := values.NewCons(
		values.NewSymbol("prefix"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.EmptyList,
		),
	)

	_, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "prefix")
}

func TestParseImportSetFromDatum_Prefix_NotASymbol(t *testing.T) {
	// (prefix (scheme base) 42)
	importSet := values.NewCons(
		values.NewSymbol("prefix"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.NewCons(values.NewInteger(42), values.EmptyList),
		),
	)

	_, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "prefix must be a symbol")
}

func TestParseImportSetFromDatum_Rename_InvalidPair(t *testing.T) {
	// (rename (scheme base) 42)
	importSet := values.NewCons(
		values.NewSymbol("rename"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.NewCons(values.NewInteger(42), values.EmptyList),
		),
	)

	_, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "expected (old new) pair")
}

func TestParseImportSetFromDatum_Rename_OldNotSymbol(t *testing.T) {
	// (rename (scheme base) (42 new))
	importSet := values.NewCons(
		values.NewSymbol("rename"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.NewCons(
				values.NewCons(
					values.NewInteger(42),
					values.NewCons(values.NewSymbol("new"), values.EmptyList),
				),
				values.EmptyList,
			),
		),
	)

	_, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "old name must be symbol")
}

func TestParseImportSetFromDatum_Rename_NewNotSymbol(t *testing.T) {
	// (rename (scheme base) (old 42))
	importSet := values.NewCons(
		values.NewSymbol("rename"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.NewCons(
				values.NewCons(
					values.NewSymbol("old"),
					values.NewCons(values.NewInteger(42), values.EmptyList),
				),
				values.EmptyList,
			),
		),
	)

	_, err := ParseImportSetFromDatum(importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "new name must be symbol")
}

func TestParseIdentifierListFromDatum(t *testing.T) {
	// (car cdr cons)
	list := values.NewCons(
		values.NewSymbol("car"),
		values.NewCons(
			values.NewSymbol("cdr"),
			values.NewCons(values.NewSymbol("cons"), values.EmptyList),
		),
	)

	result, err := parseIdentifierListFromDatum(list)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.DeepEquals, []string{"car", "cdr", "cons"})
}

func TestParseIdentifierListFromDatum_Empty(t *testing.T) {
	result, err := parseIdentifierListFromDatum(values.EmptyList)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.IsNil)
}

func TestParseIdentifierListFromDatum_NotAPair(t *testing.T) {
	_, err := parseIdentifierListFromDatum(values.NewInteger(42))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "expected list of identifiers")
}

func TestParseIdentifierListFromDatum_NotASymbol(t *testing.T) {
	// (car 42)
	list := values.NewCons(
		values.NewSymbol("car"),
		values.NewCons(values.NewInteger(42), values.EmptyList),
	)

	_, err := parseIdentifierListFromDatum(list)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "expected identifier symbol")
}
