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
	"fmt"

	"wile/values"
)

// ParseLibraryNameFromDatum extracts a LibraryName from a datum list like (scheme base).
// This is for runtime use by the 'environment' procedure.
func ParseLibraryNameFromDatum(expr values.Value) (LibraryName, error) {
	pair, ok := expr.(*values.Pair)
	if !ok {
		return LibraryName{}, values.WrapForeignErrorf(values.ErrNotAPair, "library name must be a list")
	}

	var parts []string
	_, err := pair.ForEach(nil, func(i int, hasNext bool, partExpr values.Value) error {
		if sym, ok := partExpr.(*values.Symbol); ok {
			parts = append(parts, sym.Key)
			return nil
		}
		if num, ok := partExpr.(*values.Integer); ok {
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
func ParseImportSetFromDatum(expr values.Value) (*ImportSet, error) {
	pair, ok := expr.(*values.Pair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "import set must be a list")
	}

	// Check if first element is a modifier keyword
	car := pair.Car()
	if carSym, ok := car.(*values.Symbol); ok {
		switch carSym.Key {
		case "only":
			return parseImportSetOnlyFromDatum(pair)
		case "except":
			return parseImportSetExceptFromDatum(pair)
		case "prefix":
			return parseImportSetPrefixFromDatum(pair)
		case "rename":
			return parseImportSetRenameFromDatum(pair)
		}
	}

	// Not a modifier, must be a library name
	libName, err := ParseLibraryNameFromDatum(expr)
	if err != nil {
		return nil, err
	}
	return NewImportSet(libName), nil
}

// parseImportSetOnlyFromDatum parses (only <import-set> <id> ...)
func parseImportSetOnlyFromDatum(pair *values.Pair) (*ImportSet, error) {
	cdr, ok := pair.Cdr().(*values.Pair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "only: expected import-set and identifiers")
	}

	// Get nested import set
	nestedExpr := cdr.Car()
	importSet, err := ParseImportSetFromDatum(nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get identifiers
	idsExpr := cdr.Cdr()
	ids, err := parseIdentifierListFromDatum(idsExpr)
	if err != nil {
		return nil, err
	}

	importSet.Only = ids
	return importSet, nil
}

// parseImportSetExceptFromDatum parses (except <import-set> <id> ...)
func parseImportSetExceptFromDatum(pair *values.Pair) (*ImportSet, error) {
	cdr, ok := pair.Cdr().(*values.Pair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "except: expected import-set and identifiers")
	}

	// Get nested import set
	nestedExpr := cdr.Car()
	importSet, err := ParseImportSetFromDatum(nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get identifiers
	idsExpr := cdr.Cdr()
	ids, err := parseIdentifierListFromDatum(idsExpr)
	if err != nil {
		return nil, err
	}

	importSet.Except = ids
	return importSet, nil
}

// parseImportSetPrefixFromDatum parses (prefix <import-set> <prefix>)
func parseImportSetPrefixFromDatum(pair *values.Pair) (*ImportSet, error) {
	cdr, ok := pair.Cdr().(*values.Pair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "prefix: expected import-set and prefix")
	}

	// Get nested import set
	nestedExpr := cdr.Car()
	importSet, err := ParseImportSetFromDatum(nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get prefix
	prefixPair, ok := cdr.Cdr().(*values.Pair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "prefix: expected prefix identifier")
	}

	prefixSym, ok := prefixPair.Car().(*values.Symbol)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASymbol, "prefix: prefix must be a symbol")
	}

	importSet.Prefix = prefixSym.Key
	return importSet, nil
}

// parseImportSetRenameFromDatum parses (rename <import-set> (<old> <new>) ...)
func parseImportSetRenameFromDatum(pair *values.Pair) (*ImportSet, error) {
	cdr, ok := pair.Cdr().(*values.Pair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected import-set and rename pairs")
	}

	// Get nested import set
	nestedExpr := cdr.Car()
	importSet, err := ParseImportSetFromDatum(nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get rename pairs
	renamesExpr := cdr.Cdr()
	if values.IsEmptyList(renamesExpr) {
		return importSet, nil
	}

	renamesPair, ok := renamesExpr.(*values.Pair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected list of rename pairs")
	}

	_, err = renamesPair.ForEach(nil, func(i int, hasNext bool, renamePairVal values.Value) error {
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

// parseIdentifierListFromDatum parses a list of identifiers into a string slice.
func parseIdentifierListFromDatum(expr values.Value) ([]string, error) {
	if values.IsEmptyList(expr) {
		return nil, nil
	}

	pair, ok := expr.(*values.Pair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "expected list of identifiers")
	}

	var ids []string
	_, err := pair.ForEach(nil, func(i int, hasNext bool, idExpr values.Value) error {
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
