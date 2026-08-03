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
	"bufio"
	"cmp"
	"context"
	"errors"
	"io"
	"maps"
	"slices"

	"github.com/aalpar/wile/pkg/machine/compilation/resolver"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// LibrarySummary holds the statically-extracted metadata for a single library
// definition file (.sld). It captures exports and description without compiling
// or executing any library code.
type LibrarySummary struct {
	Name        LibraryName
	Description string
	Exports     []string
	SourceFile  string
}

// LibraryExportIndex maps library keys to their summaries.
// Immutable after construction.
type LibraryExportIndex struct {
	entries map[string]*LibrarySummary
}

// NewLibraryExportIndexFromEntries creates an index from pre-built entries.
func NewLibraryExportIndexFromEntries(entries map[string]*LibrarySummary) *LibraryExportIndex {
	return &LibraryExportIndex{entries: entries}
}

// Lookup returns the summary for a library, or nil if not indexed.
func (p *LibraryExportIndex) Lookup(name LibraryName) *LibrarySummary {
	if p == nil {
		return nil
	}
	return p.entries[name.Key()]
}

// Entries returns all indexed summaries sorted by library key.
func (p *LibraryExportIndex) Entries() []*LibrarySummary {
	if p == nil {
		return nil
	}
	return slices.SortedFunc(maps.Values(p.entries), func(a, b *LibrarySummary) int {
		return cmp.Compare(a.Name.Key(), b.Name.Key())
	})
}

// ParseLibrarySummary reads a library definition from r and extracts its
// export list and description without compiling the library. This is a
// best-effort parser: malformed export specs are silently skipped.
func ParseLibrarySummary(ctx context.Context, r io.Reader, filePath string, name LibraryName) (*LibrarySummary, error) {
	p := parser.NewParserWithFile(nil, false, bufio.NewReader(r), filePath)

	stx, err := p.ReadSyntax(ctx)
	if err != nil {
		if errors.Is(err, io.EOF) {
			return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "define-library: file is empty: %s", filePath)
		}
		return nil, werr.WrapForeignErrorf(err, "define-library: could not parse: %s", filePath)
	}

	pair, ok := stx.(*syntax.SyntaxPair)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "define-library: expected list form, got %T", stx)
	}

	carStx := pair.SyntaxCar()
	carSym, ok := carStx.(*syntax.SyntaxSymbol)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "define-library: expected keyword, got %T", carStx)
	}

	symName := carSym.Key()
	if symName != "define-library" && symName != "library" {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "define-library: expected define-library or library, got %s", symName)
	}

	// Require and validate the library name (2nd element).
	cdr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed,
			"define-library: missing library name in %s", filePath)
	}

	parsedName, err := ParseLibraryNameFromDatum(ctx, cdr.SyntaxCar().UnwrapAll())
	if err != nil {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed,
			"define-library: malformed library name in %s", filePath)
	}
	if parsedName.Key() != name.Key() {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed,
			"define-library: name mismatch in %s: expected %s, got %s",
			filePath, name.SchemeString(), parsedName.SchemeString())
	}

	q := &LibrarySummary{
		Name:       parsedName,
		SourceFile: filePath,
	}

	// Advance past the library name to the declarations.
	declList := cdr.SyntaxCdr()

	err = syntax.SyntaxWalk(ctx, declList, func(v syntax.SyntaxValue) error {
		return parseSummaryDeclaration(ctx, q, v)
	})
	if err != nil {
		return nil, err
	}

	return q, nil
}

// parseSummaryDeclaration dispatches on the keyword of a single library
// declaration (export, description, etc.). Unknown keywords are skipped.
func parseSummaryDeclaration(ctx context.Context, summary *LibrarySummary, decl syntax.SyntaxValue) error {
	switch d := decl.(type) {
	case *syntax.SyntaxComment, *syntax.SyntaxDatumComment:
		return nil

	case *syntax.SyntaxPair:
		carStx := d.SyntaxCar()
		carSym, ok := carStx.(*syntax.SyntaxSymbol)
		if !ok {
			// Not a keyword-headed declaration; skip.
			return nil
		}

		switch carSym.Key() {
		case "export":
			return parseSummaryExports(ctx, summary, d.SyntaxCdr())
		case "description":
			parseSummaryDescription(summary, d.SyntaxCdr())
			return nil
		default:
			// begin, import, include, cond-expand, etc. — skip.
			return nil
		}

	default:
		// Unexpected declaration form; skip silently.
		return nil
	}
}

// parseSummaryExports walks the arguments of an (export ...) declaration,
// extracting each export name.
func parseSummaryExports(ctx context.Context, summary *LibrarySummary, args syntax.SyntaxValue) error {
	return syntax.SyntaxWalk(ctx, args, func(v syntax.SyntaxValue) error {
		parseSummaryExportSpec(summary, v)
		return nil
	})
}

// parseSummaryExportSpec extracts a single export name from an export spec.
// For simple symbols, the symbol key is used. For (rename internal external),
// the external name (3rd element) is extracted. Malformed specs are skipped.
func parseSummaryExportSpec(summary *LibrarySummary, spec syntax.SyntaxValue) {
	switch s := spec.(type) {
	case *syntax.SyntaxComment, *syntax.SyntaxDatumComment:
		return

	case *syntax.SyntaxSymbol:
		summary.Exports = append(summary.Exports, s.Key())

	case *syntax.SyntaxPair:
		// Could be (rename internal external).
		carSym, ok := s.SyntaxCar().(*syntax.SyntaxSymbol)
		if !ok || carSym.Key() != "rename" {
			return
		}
		// (rename internal external) — exactly three elements counting the
		// keyword. Skip the spec on any structural mismatch (best-effort index).
		parts, err := syntax.FormParts(s, "rename", 3, 3)
		if err != nil {
			return
		}
		externalSym, ok := parts[2].(*syntax.SyntaxSymbol)
		if ok {
			summary.Exports = append(summary.Exports, externalSym.Key())
		}
	}
}

// parseSummaryDescription extracts the description string from a
// (description "...") declaration. If the argument is not a string,
// the description is left unchanged.
func parseSummaryDescription(summary *LibrarySummary, args syntax.SyntaxValue) {
	// (description <string>) — exactly one argument; skip on any mismatch
	// (best-effort index). Mirrors processLibraryDescription on the compile path.
	strExpr, err := formSingleArg(args, "description")
	if err != nil {
		return
	}
	descStr, ok := strExpr.UnwrapAll().(*values.String)
	if !ok {
		return
	}

	summary.Description = descStr.Value
}

// BuildExportIndex scans all discoverable library files via the resolver's
// FileEnumerator, parses their exports, and returns a LibraryExportIndex.
// Libraries already loaded in reg are skipped (their metadata is already
// available via the registry). If the resolver does not implement
// FileEnumerator, an empty index is returned (not an error).
//
// Best-effort: per-file name and parse errors are joined and returned alongside
// a valid (partial) index; only an EnumerateFiles failure or a cancelled context
// returns a nil index.
func BuildExportIndex(ctx context.Context, res FileResolver, reg *LibraryRegistry) (*LibraryExportIndex, error) {
	fileEnum, ok := res.(resolver.FileEnumerator)
	if !ok {
		return NewLibraryExportIndexFromEntries(nil), nil
	}

	files, err := fileEnum.EnumerateFiles()
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "library-index: enumerate files")
	}

	entries := make(map[string]*LibrarySummary)
	var pathErrs []error
	for _, path := range files {
		err = ctx.Err()
		if err != nil {
			return nil, werr.WrapForeignErrorf(err, "library-index: context cancelled")
		}
		name, nameErr := FilePathToLibraryName(path)
		if nameErr != nil {
			pathErrs = append(pathErrs, nameErr)
			continue
		}
		key := name.Key()
		if entries[key] != nil {
			continue
		}
		if reg != nil && reg.Lookup(name) != nil {
			continue
		}
		summary, parseErr := tryParseLibrary(ctx, res, name)
		if parseErr != nil {
			pathErrs = append(pathErrs, parseErr)
			continue
		}
		if summary == nil {
			continue
		}
		entries[key] = summary
	}

	return NewLibraryExportIndexFromEntries(entries), errors.Join(pathErrs...)
}

// tryParseLibrary opens and parses a single library file, returning its
// summary. Tries .sld first, falling back to .scm on file-not-found.
// Returns (nil, nil) when neither file exists. Returns a non-nil error
// for security denials, I/O failures, or parse errors.
func tryParseLibrary(ctx context.Context, resolver FileResolver, name LibraryName) (*LibrarySummary, error) {
	f, filePath, err := ResolveLibraryFile(ctx, resolver, name)
	if err != nil {
		if errors.Is(err, werr.ErrFileNotFound) {
			return nil, nil
		}
		return nil, err
	}
	defer f.Close() //nolint:errcheck

	summary, err := ParseLibrarySummary(ctx, f, filePath, name)
	if err != nil {
		return nil, err
	}
	return summary, nil
}
