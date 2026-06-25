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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

func TestParseLibraryNameFromDatum(t *testing.T) {
	// (scheme base)
	libName := values.NewCons(
		values.NewSymbol("scheme"),
		values.NewCons(values.NewSymbol("base"), values.EmptyList),
	)

	result, err := ParseLibraryNameFromDatum(context.Background(), libName)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.Key(), qt.Equals, "scheme/base")
}

// TestParseLibraryNameRejectsTraversal verifies that path-traversal and empty
// library-name parts are rejected at parse time — the single choke point for
// both compile- and runtime-constructed names — so a name like (.. .. foo)
// can never reach the OS resolver and escape the search root (S1).
func TestParseLibraryNameRejectsTraversal(t *testing.T) {
	for _, part := range []string{"..", ".", "", "a/b", "a\\b"} {
		// Leading position.
		first := values.List(values.NewSymbol(part), values.NewSymbol("foo"))
		_, err := ParseLibraryNameFromDatum(context.Background(), first)
		qt.Assert(t, errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue,
			qt.Commentf("part %q in leading position", part))

		// Non-leading position: the guard runs on every part, so a bad part
		// anywhere must be rejected (mirrors the (.. .. foo) threat shape).
		second := values.List(values.NewSymbol("foo"), values.NewSymbol(part))
		_, err = ParseLibraryNameFromDatum(context.Background(), second)
		qt.Assert(t, errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue,
			qt.Commentf("part %q in non-leading position", part))
	}
}

// TestParseLibraryNameAllowsLegitimate guards against over-rejection: ordinary
// names with a dot inside an identifier and integer parts must still parse.
func TestParseLibraryNameAllowsLegitimate(t *testing.T) {
	cases := []struct {
		datum values.Value
		key   string
	}{
		{values.List(values.NewSymbol("srfi"), values.NewInteger(1)), "srfi/1"},
		{values.List(values.NewSymbol("scheme"), values.NewSymbol("char")), "scheme/char"},
		{values.List(values.NewSymbol("a.b")), "a.b"}, // dot inside an identifier is fine
	}
	for _, tc := range cases {
		result, err := ParseLibraryNameFromDatum(context.Background(), tc.datum)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result.Key(), qt.Equals, tc.key)
	}
}

func TestParseLibraryNameFromDatum_WithNumbers(t *testing.T) {
	// (srfi 1)
	libName := values.NewCons(
		values.NewSymbol("srfi"),
		values.NewCons(values.NewInteger(1), values.EmptyList),
	)

	result, err := ParseLibraryNameFromDatum(context.Background(), libName)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.Key(), qt.Equals, "srfi/1")
}

func TestParseLibraryNameFromDatum_NotAPair(t *testing.T) {
	_, err := ParseLibraryNameFromDatum(context.Background(), values.NewInteger(42))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue)
}

func TestParseLibraryNameFromDatum_Empty(t *testing.T) {
	_, err := ParseLibraryNameFromDatum(context.Background(), values.EmptyList)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue)
}

func TestParseLibraryNameFromDatum_InvalidPart(t *testing.T) {
	// (scheme "invalid")
	libName := values.NewCons(
		values.NewSymbol("scheme"),
		values.NewCons(values.NewString("invalid"), values.EmptyList),
	)

	_, err := ParseLibraryNameFromDatum(context.Background(), libName)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue)
}

// TestParseLibraryNameFromDatum_RejectsImproperList pins the behavior added
// when ParseLibraryNameFromDatum migrated to values.ForEachProperList: a
// dotted-tail library name like (scheme . base) is no longer silently
// accepted as if the trailing element did not exist.
func TestParseLibraryNameFromDatum_RejectsImproperList(t *testing.T) {
	tcs := []struct {
		name string
		expr values.Value
	}{
		{
			name: "dotted-pair",
			expr: values.NewCons(values.NewSymbol("scheme"), values.NewSymbol("base")),
		},
		{
			name: "trailing-non-list",
			expr: values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.NewInteger(1)),
			),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := ParseLibraryNameFromDatum(context.Background(), tc.expr)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue,
				qt.Commentf("expected ErrNotAList, got %v", err))
		})
	}
}

func TestParseImportSetFromDatum_Simple(t *testing.T) {
	// (scheme base)
	importSet := values.NewCons(
		values.NewSymbol("scheme"),
		values.NewCons(values.NewSymbol("base"), values.EmptyList),
	)

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Modifiers, qt.HasLen, 0)
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

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Modifiers, qt.HasLen, 1)
	qt.Assert(t, result.Modifiers[0].kind, qt.Equals, importModOnly)
	qt.Assert(t, result.Modifiers[0].ids, qt.DeepEquals, map[string]struct{}{"car": {}, "cdr": {}})
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

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Modifiers, qt.HasLen, 1)
	qt.Assert(t, result.Modifiers[0].kind, qt.Equals, importModExcept)
	qt.Assert(t, result.Modifiers[0].ids, qt.DeepEquals, map[string]struct{}{"car": {}, "cdr": {}})
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

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Modifiers, qt.HasLen, 1)
	qt.Assert(t, result.Modifiers[0].kind, qt.Equals, importModPrefix)
	qt.Assert(t, result.Modifiers[0].prefix, qt.Equals, "scheme:")
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

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.Modifiers, qt.HasLen, 1)
	qt.Assert(t, result.Modifiers[0].kind, qt.Equals, importModRename)
	qt.Assert(t, result.Modifiers[0].renames["car"], qt.Equals, "first")
	qt.Assert(t, result.Modifiers[0].renames["cdr"], qt.Equals, "rest")
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

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	// Nested modifiers are preserved as an ordered, innermost-first list — NOT
	// flattened onto one struct. (prefix (only … car cdr) scheme:) ⇒ [only, prefix],
	// which ApplyToExports folds inside-out (filter to {car,cdr}, then prefix). The
	// old flat representation collapsed this to Only={car,cdr}+Prefix=scheme:, losing
	// the ordering between different modifier kinds (libraries-plan Task 5A / 7D).
	qt.Assert(t, result.Modifiers, qt.HasLen, 2)
	qt.Assert(t, result.Modifiers[0].kind, qt.Equals, importModOnly)
	qt.Assert(t, result.Modifiers[0].ids, qt.DeepEquals, map[string]struct{}{"car": {}, "cdr": {}})
	qt.Assert(t, result.Modifiers[1].kind, qt.Equals, importModPrefix)
	qt.Assert(t, result.Modifiers[1].prefix, qt.Equals, "scheme:")
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

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	// (only LIB) with zero identifiers installs an `only` modifier selecting nothing —
	// R7RS §5.6 reads <identifier> … as zero-or-more, so the empty subset imports
	// nothing (NOT "no filter / import everything").
	qt.Assert(t, result.Modifiers, qt.HasLen, 1)
	qt.Assert(t, result.Modifiers[0].kind, qt.Equals, importModOnly)
	qt.Assert(t, result.Modifiers[0].ids, qt.HasLen, 0)
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

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	// An empty `rename` adds no modifier.
	qt.Assert(t, result.Modifiers, qt.HasLen, 0)
}

func TestParseImportSetFromDatum_NotAPair(t *testing.T) {
	_, err := ParseImportSetFromDatum(context.Background(), values.NewInteger(42))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue)
}

func TestParseImportSetFromDatum_Only_InvalidFormat(t *testing.T) {
	// (only)
	importSet := values.NewCons(
		values.NewSymbol("only"),
		values.EmptyList,
	)

	_, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue)
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

	_, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue)
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

	_, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotASymbol), qt.IsTrue)
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

	_, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue)
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

	_, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotASymbol), qt.IsTrue)
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

	_, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotASymbol), qt.IsTrue)
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

	result, err := parseIdentifierListFromDatum(context.Background(), list)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.DeepEquals, map[string]struct{}{"car": {}, "cdr": {}, "cons": {}})
}

func TestParseIdentifierListFromDatum_Empty(t *testing.T) {
	result, err := parseIdentifierListFromDatum(context.Background(), values.EmptyList)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.IsNil)
}

func TestParseIdentifierListFromDatum_NotAPair(t *testing.T) {
	_, err := parseIdentifierListFromDatum(context.Background(), values.NewInteger(42))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue)
}

func TestParseIdentifierListFromDatum_NotASymbol(t *testing.T) {
	// (car 42)
	list := values.NewCons(
		values.NewSymbol("car"),
		values.NewCons(values.NewInteger(42), values.EmptyList),
	)

	_, err := parseIdentifierListFromDatum(context.Background(), list)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotASymbol), qt.IsTrue)
}

// Phase shift tests

func TestParseImportSetFromDatum_ForSyntax(t *testing.T) {
	// (for-syntax (scheme base))
	importSet := values.NewCons(
		values.NewSymbol("for-syntax"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.EmptyList,
		),
	)

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.PhaseShift, qt.Equals, environment.Phase(1))
}

func TestParseImportSetFromDatum_ForTemplate(t *testing.T) {
	// (for-template (scheme base))
	importSet := values.NewCons(
		values.NewSymbol("for-template"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("scheme"),
				values.NewCons(values.NewSymbol("base"), values.EmptyList),
			),
			values.EmptyList,
		),
	)

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.PhaseShift, qt.Equals, environment.Phase(-1))
}

func TestParseImportSetFromDatum_ForMeta(t *testing.T) {
	// (for-meta 2 (scheme base))
	importSet := values.NewCons(
		values.NewSymbol("for-meta"),
		values.NewCons(
			values.NewInteger(2),
			values.NewCons(
				values.NewCons(
					values.NewSymbol("scheme"),
					values.NewCons(values.NewSymbol("base"), values.EmptyList),
				),
				values.EmptyList,
			),
		),
	)

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.PhaseShift, qt.Equals, environment.Phase(2))
}

func TestParseImportSetFromDatum_ForMetaNegative(t *testing.T) {
	// (for-meta -1 (scheme base))
	importSet := values.NewCons(
		values.NewSymbol("for-meta"),
		values.NewCons(
			values.NewInteger(-1),
			values.NewCons(
				values.NewCons(
					values.NewSymbol("scheme"),
					values.NewCons(values.NewSymbol("base"), values.EmptyList),
				),
				values.EmptyList,
			),
		),
	)

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.PhaseShift, qt.Equals, environment.Phase(-1))
}

func TestParseImportSetFromDatum_NestedForSyntax(t *testing.T) {
	// (for-syntax (for-syntax (scheme base))) should be phase +2
	importSet := values.NewCons(
		values.NewSymbol("for-syntax"),
		values.NewCons(
			values.NewCons(
				values.NewSymbol("for-syntax"),
				values.NewCons(
					values.NewCons(
						values.NewSymbol("scheme"),
						values.NewCons(values.NewSymbol("base"), values.EmptyList),
					),
					values.EmptyList,
				),
			),
			values.EmptyList,
		),
	)

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.PhaseShift, qt.Equals, environment.Phase(2))
}

func TestParseImportSetFromDatum_ForSyntaxWithOnly(t *testing.T) {
	// (for-syntax (only (scheme base) car cdr))
	importSet := values.NewCons(
		values.NewSymbol("for-syntax"),
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
			values.EmptyList,
		),
	)

	result, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.LibraryName.Key(), qt.Equals, "scheme/base")
	qt.Assert(t, result.PhaseShift, qt.Equals, environment.Phase(1))
	qt.Assert(t, result.Modifiers, qt.HasLen, 1)
	qt.Assert(t, result.Modifiers[0].kind, qt.Equals, importModOnly)
	qt.Assert(t, result.Modifiers[0].ids, qt.DeepEquals, map[string]struct{}{"car": {}, "cdr": {}})
}

func TestParseImportSetFromDatum_ForSyntax_InvalidFormat(t *testing.T) {
	// (for-syntax) - missing import set
	importSet := values.NewCons(
		values.NewSymbol("for-syntax"),
		values.EmptyList,
	)

	_, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue)
}

func TestParseImportSetFromDatum_ForMeta_InvalidFormat(t *testing.T) {
	// (for-meta) - missing phase level
	importSet := values.NewCons(
		values.NewSymbol("for-meta"),
		values.EmptyList,
	)

	_, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue)
}

func TestParseImportSetFromDatum_ForMeta_NotInteger(t *testing.T) {
	// (for-meta "bad" (scheme base))
	importSet := values.NewCons(
		values.NewSymbol("for-meta"),
		values.NewCons(
			values.NewString("bad"),
			values.NewCons(
				values.NewCons(
					values.NewSymbol("scheme"),
					values.NewCons(values.NewSymbol("base"), values.EmptyList),
				),
				values.EmptyList,
			),
		),
	)

	_, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAnInteger), qt.IsTrue)
}

func TestParseImportSetFromDatum_ForMeta_MissingImportSet(t *testing.T) {
	// (for-meta 1) - missing import set after phase level
	importSet := values.NewCons(
		values.NewSymbol("for-meta"),
		values.NewCons(
			values.NewInteger(1),
			values.EmptyList,
		),
	)

	_, err := ParseImportSetFromDatum(context.Background(), importSet)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue)
}
