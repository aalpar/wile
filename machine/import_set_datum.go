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
	"context"
	"fmt"

	"github.com/aalpar/wile/values"
)

// ParseLibraryNameFromDatum extracts a LibraryName from a datum list like (scheme base).
// This is for runtime use by the 'environment' procedure.
func ParseLibraryNameFromDatum(ctx context.Context, expr values.Value) (LibraryName, error) {
	if values.IsEmptyList(expr) {
		return LibraryName{}, values.NewForeignErrorf("library name cannot be empty")
	}
	pair, ok := expr.(*values.Pair)
	if !ok {
		return LibraryName{}, values.WrapForeignErrorf(values.ErrNotAPair, "library name must be a list")
	}

	var parts []string
	_, err := pair.ForEach(ctx, func(_ context.Context, _ int, _ bool, partExpr values.Value) error {
		sym, ok := partExpr.(*values.Symbol)
		if ok {
			parts = append(parts, sym.Key)
			return nil
		}
		num, ok := partExpr.(*values.Integer)
		if ok {
			parts = append(parts, fmt.Sprintf("%d", num.Value))
			return nil
		}
		return values.NewForeignErrorf("library name part must be identifier or integer, got %T", partExpr)
	})
	if err != nil {
		return LibraryName{}, err
	}
	if len(parts) == 0 {
		return LibraryName{}, values.NewForeignErrorf("library name cannot be empty")
	}
	return NewLibraryName(parts...), nil
}

// ParseImportSetFromDatum parses an import set from a datum value.
// This is for runtime use by the 'environment' procedure.
// Import sets can be:
//   - (<library-name>)              : import all exports
//   - (only <import-set> <id> ...)  : import only specified identifiers
//   - (except <import-set> <id> ...): import all except specified
//   - (prefix <import-set> <prefix>): add prefix to all imported names
//   - (rename <import-set> (<old> <new>) ...): rename specific imports
//   - (for-syntax <import-set>)     : import at phase +1 (macro expansion)
//   - (for-template <import-set>)   : import at phase -1
//   - (for-meta <n> <import-set>)   : import at phase +n
func ParseImportSetFromDatum(ctx context.Context, expr values.Value) (*ImportSet, error) {
	if values.IsEmptyList(expr) {
		return nil, values.NewForeignErrorf("import set cannot be empty")
	}
	pair, ok := expr.(*values.Pair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "import set must be a list")
	}

	// Check if first element is a modifier keyword
	car := pair.Car()
	carSym, ok := car.(*values.Symbol)
	if ok {
		switch carSym.Key {
		case "only":
			return parseImportSetOnlyFromDatum(ctx, pair)
		case "except":
			return parseImportSetExceptFromDatum(ctx, pair)
		case "prefix":
			return parseImportSetPrefixFromDatum(ctx, pair)
		case "rename":
			return parseImportSetRenameFromDatum(ctx, pair)
		case "for-syntax":
			return parseImportSetForSyntaxFromDatum(ctx, pair)
		case "for-template":
			return parseImportSetForTemplateFromDatum(ctx, pair)
		case "for-meta":
			return parseImportSetForMetaFromDatum(ctx, pair)
		}
	}

	// Not a modifier, must be a library name
	libName, err := ParseLibraryNameFromDatum(ctx, expr)
	if err != nil {
		return nil, err
	}
	return NewImportSet(libName), nil
}

// parseImportSetOnlyFromDatum parses (only <import-set> <id> ...)
func parseImportSetOnlyFromDatum(ctx context.Context, tuple values.Tuple) (*ImportSet, error) {
	cdr := tuple.Cdr()
	if values.IsEmptyList(cdr) {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "only: expected import-set and identifiers")
	}

	cdrTuple, ok := cdr.(values.Tuple)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "only: expected a list")
	}

	// Get nested import set
	nestedExpr := cdrTuple.Car()
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get identifiers
	idsExpr := cdrTuple.Cdr()
	ids, err := parseIdentifierListFromDatum(ctx, idsExpr)
	if err != nil {
		return nil, err
	}

	importSet.Only = ids
	return importSet, nil
}

// parseImportSetExceptFromDatum parses (except <import-set> <id> ...)
func parseImportSetExceptFromDatum(ctx context.Context, tuple values.Tuple) (*ImportSet, error) {
	cdr := tuple.Cdr()
	if values.IsEmptyList(cdr) {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "except: expected import-set and identifiers")
	}

	cdrTuple, ok := cdr.(values.Tuple)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "except: expected a list")
	}

	// Get nested import set
	nestedExpr := cdrTuple.Car()
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get identifiers
	idsExpr := cdrTuple.Cdr()
	ids, err := parseIdentifierListFromDatum(ctx, idsExpr)
	if err != nil {
		return nil, err
	}

	importSet.Except = ids
	return importSet, nil
}

// parseImportSetPrefixFromDatum parses (prefix <import-set> <prefix>)
func parseImportSetPrefixFromDatum(ctx context.Context, tuple values.Tuple) (*ImportSet, error) {
	cdr := tuple.Cdr()
	if values.IsEmptyList(cdr) {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "prefix: expected import-set and prefix")
	}

	cdrTuple, ok := cdr.(values.Tuple)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "prefix: expected a list")
	}

	// Get nested import set
	nestedExpr := cdrTuple.Car()
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get prefix
	prefixValue := cdrTuple.Cdr()
	if values.IsEmptyList(prefixValue) {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "prefix: expected prefix identifier")
	}

	prefixTuple, ok := prefixValue.(values.Tuple)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "prefix: expected prefix identifier")
	}

	prefixSym, ok := prefixTuple.Car().(*values.Symbol)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASymbol, "prefix: prefix must be a symbol")
	}

	importSet.Prefix = prefixSym.Key
	return importSet, nil
}

// parseImportSetRenameFromDatum parses (rename <import-set> (<old> <new>) ...)
func parseImportSetRenameFromDatum(ctx context.Context, tuple values.Tuple) (*ImportSet, error) {
	cdr := tuple.Cdr()
	if values.IsEmptyList(cdr) {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "rename: expected import-set and rename pairs")
	}

	cdrTuple, ok := cdr.(values.Tuple)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "rename: expected a list")
	}

	// Get nested import set
	nestedExpr := cdrTuple.Car()
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get rename pairs
	renamesExpr := cdrTuple.Cdr()
	if values.IsEmptyList(renamesExpr) {
		return importSet, nil
	}

	renamesTuple, ok := renamesExpr.(values.Tuple)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "rename: expected list of rename pairs")
	}

	_, err = renamesTuple.ForEach(ctx, func(_ context.Context, _ int, _ bool, renamePairVal values.Value) error {
		// Each rename element must be a structural pair (old new), not just a Tuple
		renamePair, ok := renamePairVal.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected (old new) pair")
		}

		oldSym, ok := renamePair.Car().(*values.Symbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASymbol, "rename: old name must be symbol")
		}

		newPair, ok := renamePair.Cdr().(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected new name")
		}

		newSym, ok := newPair.Car().(*values.Symbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASymbol, "rename: new name must be symbol")
		}

		importSet.Renames[oldSym.Key] = newSym.Key
		return nil
	})

	return importSet, err
}

// parseImportSetForSyntaxFromDatum parses (for-syntax <import-set>)
// Adds +1 to the phase shift of the nested import set.
func parseImportSetForSyntaxFromDatum(ctx context.Context, tuple values.Tuple) (*ImportSet, error) {
	cdr := tuple.Cdr()
	if values.IsEmptyList(cdr) {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "for-syntax: expected import-set")
	}

	cdrTuple, ok := cdr.(values.Tuple)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "for-syntax: expected a list")
	}

	// Get nested import set
	nestedExpr := cdrTuple.Car()
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Add +1 to phase shift (composable)
	importSet.PhaseShift++
	return importSet, nil
}

// parseImportSetForTemplateFromDatum parses (for-template <import-set>)
// Adds -1 to the phase shift of the nested import set.
func parseImportSetForTemplateFromDatum(ctx context.Context, tuple values.Tuple) (*ImportSet, error) {
	cdr := tuple.Cdr()
	if values.IsEmptyList(cdr) {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "for-template: expected import-set")
	}

	cdrTuple, ok := cdr.(values.Tuple)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "for-template: expected a list")
	}

	// Get nested import set
	nestedExpr := cdrTuple.Car()
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Add -1 to phase shift (composable)
	importSet.PhaseShift--
	return importSet, nil
}

// parseImportSetForMetaFromDatum parses (for-meta <n> <import-set>)
// Adds n to the phase shift of the nested import set.
func parseImportSetForMetaFromDatum(ctx context.Context, tuple values.Tuple) (*ImportSet, error) {
	cdr := tuple.Cdr()
	if values.IsEmptyList(cdr) {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "for-meta: expected phase level and import-set")
	}

	cdrTuple, ok := cdr.(values.Tuple)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "for-meta: expected a list")
	}

	// Get phase level (integer)
	phaseExpr := cdrTuple.Car()
	phaseInt, ok := phaseExpr.(*values.Integer)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAnInteger, "for-meta: expected integer phase level")
	}

	// Get nested import set
	importSetValue := cdrTuple.Cdr()
	if values.IsEmptyList(importSetValue) {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "for-meta: expected import-set after phase level")
	}

	importSetTuple, ok := importSetValue.(values.Tuple)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "for-meta: expected import-set after phase level")
	}

	nestedExpr := importSetTuple.Car()
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Add n to phase shift (composable)
	importSet.PhaseShift += int(phaseInt.Value)
	return importSet, nil
}

// parseIdentifierListFromDatum parses a list of identifiers into a string slice.
func parseIdentifierListFromDatum(ctx context.Context, expr values.Value) ([]string, error) {
	if values.IsEmptyList(expr) {
		return nil, nil
	}

	tuple, ok := expr.(values.Tuple)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAList, "expected list of identifiers")
	}

	var ids []string
	_, err := tuple.ForEach(ctx, func(_ context.Context, i int, hasNext bool, idExpr values.Value) error {
		idSym, ok := idExpr.(*values.Symbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASymbol, "expected identifier symbol")
		}
		ids = append(ids, idSym.Key)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return ids, nil
}
