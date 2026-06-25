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
	"fmt"
	"math"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ParseLibraryNameFromDatum extracts a LibraryName from a datum list like (scheme base).
// Used at both runtime (by the 'environment' procedure) and compile time
// (via UnwrapAll on syntax objects).
func ParseLibraryNameFromDatum(ctx context.Context, expr values.Value) (LibraryName, error) {
	if values.IsEmptyList(expr) {
		return LibraryName{}, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "library name cannot be empty")
	}
	tuple, ok := expr.(values.Tuple)
	if !ok {
		return LibraryName{}, werr.WrapForeignErrorf(werr.ErrNotAList, "library name must be a list")
	}

	var parts []string
	err := values.ForEachProperList(ctx, tuple, "library name", func(_ context.Context, _ int, _ bool, partExpr values.Value) error {
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
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "library name part must be identifier or integer, got %T", partExpr)
	})
	if err != nil {
		return LibraryName{}, err
	}
	if len(parts) == 0 {
		return LibraryName{}, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "library name cannot be empty")
	}
	return NewLibraryName(parts...), nil
}

// ParseImportSetFromDatum parses an import set from datum values.
// Used at both runtime (by the 'environment' procedure) and compile time
// (via UnwrapAll on syntax objects).
//
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
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "import set cannot be empty")
	}
	tuple, ok := expr.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList, "import set must be a list")
	}

	// Check if first element is a modifier keyword
	car := tuple.Car()
	carSym, ok := car.(*values.Symbol)
	if ok {
		switch carSym.Key {
		case "only":
			return parseImportSetFilterFromDatum(ctx, "only", tuple, (*ImportSet).AddOnly)
		case "except":
			return parseImportSetFilterFromDatum(ctx, "except", tuple, (*ImportSet).AddExcept)
		case "prefix":
			return parseImportSetPrefixFromDatum(ctx, tuple)
		case "rename":
			return parseImportSetRenameFromDatum(ctx, tuple)
		case "for-syntax":
			return parseImportSetPhaseShiftFromDatum(ctx, "for-syntax", tuple, 1)
		case "for-template":
			return parseImportSetPhaseShiftFromDatum(ctx, "for-template", tuple, -1)
		case "for-meta":
			return parseImportSetForMetaFromDatum(ctx, tuple)
		}
	}

	// Not a modifier, must be a library name
	libName, err := ParseLibraryNameFromDatum(ctx, expr)
	if err != nil {
		return nil, err
	}
	return NewImportSet(libName), nil
}

// parseImportSetFilterFromDatum parses (<keyword> <import-set> <id> ...) where
// the keyword is "only" or "except". The apply callback (AddOnly or AddExcept)
// appends the corresponding modifier carrying the parsed identifier list.
func parseImportSetFilterFromDatum(
	ctx context.Context, keyword string, tuple values.Tuple,
	apply func(*ImportSet, map[string]struct{}),
) (*ImportSet, error) {
	nestedExpr, idsExpr, err := values.Uncons(tuple.Cdr(), keyword, "import-set and identifiers")
	if err != nil {
		return nil, err
	}
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}
	ids, err := parseIdentifierListFromDatum(ctx, idsExpr)
	if err != nil {
		return nil, err
	}
	apply(importSet, ids)
	return importSet, nil
}

// parseImportSetPrefixFromDatum parses (prefix <import-set> <prefix>)
func parseImportSetPrefixFromDatum(ctx context.Context, tuple values.Tuple) (*ImportSet, error) {
	nestedExpr, prefixValue, err := values.Uncons(tuple.Cdr(), "prefix", "import-set and prefix")
	if err != nil {
		return nil, err
	}
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}
	prefixSym, _, err := values.UnconsTyped[*values.Symbol](
		prefixValue, werr.ErrNotASymbol, "prefix", "prefix identifier")
	if err != nil {
		return nil, err
	}
	importSet.AddPrefix(prefixSym.Key)
	return importSet, nil
}

// parseImportSetRenameFromDatum parses (rename <import-set> (<old> <new>) ...)
func parseImportSetRenameFromDatum(ctx context.Context, tuple values.Tuple) (*ImportSet, error) {
	nestedExpr, renamesExpr, err := values.Uncons(tuple.Cdr(), "rename", "import-set and rename pairs")
	if err != nil {
		return nil, err
	}
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}
	if values.IsEmptyList(renamesExpr) {
		return importSet, nil
	}
	renamesTuple, ok := renamesExpr.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList, "rename: expected list of rename pairs")
	}

	renames := make(map[string]string)
	err = values.ForEachProperList(ctx, renamesTuple, "rename", func(_ context.Context, _ int, _ bool, renamePairVal values.Value) error {
		oldSym, newRest, err := values.UnconsTyped[*values.Symbol](renamePairVal, werr.ErrNotASymbol, "rename", "old name")
		if err != nil {
			return err
		}
		newSym, _, err := values.UnconsTyped[*values.Symbol](newRest, werr.ErrNotASymbol, "rename", "new name")
		if err != nil {
			return err
		}
		renames[oldSym.Key] = newSym.Key
		return nil
	})
	if err != nil {
		return nil, err
	}

	importSet.AddRename(renames)
	return importSet, nil
}

// parseImportSetPhaseShiftFromDatum parses (<keyword> <import-set>) and adds
// delta to the nested import set's phase shift. Handles for-syntax (+1) and
// for-template (-1).
func parseImportSetPhaseShiftFromDatum(ctx context.Context, keyword string, tuple values.Tuple, delta environment.Phase) (*ImportSet, error) {
	nestedExpr, _, err := values.Uncons(tuple.Cdr(), keyword, "import-set")
	if err != nil {
		return nil, err
	}
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}
	importSet.PhaseShift += delta
	return importSet, nil
}

// parseImportSetForMetaFromDatum parses (for-meta <n> <import-set>)
// Adds n to the phase shift of the nested import set.
func parseImportSetForMetaFromDatum(ctx context.Context, tuple values.Tuple) (*ImportSet, error) {
	phaseInt, importSetValue, err := values.UnconsTyped[*values.Integer](
		tuple.Cdr(), werr.ErrNotAnInteger, "for-meta", "phase level")
	if err != nil {
		return nil, err
	}
	nestedExpr, _, err := values.Uncons(importSetValue, "for-meta", "import-set after phase level")
	if err != nil {
		return nil, err
	}
	importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
	if err != nil {
		return nil, err
	}

	// Add n to phase shift (composable). environment.Phase is int8; reject
	// values that would silently truncate. Composition with importSet's
	// existing PhaseShift could still overflow int8 — guard the operand
	// here; the composition guard belongs in callers that compose chains.
	n := phaseInt.Value
	if n < math.MinInt8 || n > math.MaxInt8 {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"for-meta: phase %d out of range [%d, %d]",
			n, int64(math.MinInt8), int64(math.MaxInt8))
	}
	importSet.PhaseShift += environment.Phase(n)
	return importSet, nil
}

// parseIdentifierListFromDatum parses a list of identifiers into a name set.
func parseIdentifierListFromDatum(ctx context.Context, expr values.Value) (map[string]struct{}, error) {
	if values.IsEmptyList(expr) {
		return nil, nil
	}

	tuple, ok := expr.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList, "expected list of identifiers")
	}

	ids := make(map[string]struct{})
	err := values.ForEachProperList(ctx, tuple, "identifier list", func(_ context.Context, _ int, _ bool, idExpr values.Value) error {
		idSym, ok := idExpr.(*values.Symbol)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotASymbol, "expected identifier symbol")
		}
		ids[idSym.Key] = struct{}{}
		return nil
	})
	if err != nil {
		return nil, err
	}
	return ids, nil
}
