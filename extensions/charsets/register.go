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

package charsets

import (
	"github.com/aalpar/wile/pkg/registry"
)

// Extension is the SRFI-14 Character-Set Library extension.
var Extension = registry.NewDescribedExtension("charsets",
	"SRFI-14 Character-Set Library: char-set value type, set algebra, named char-sets.",
	AddToRegistry)

// Builder aggregates all charsets registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all charset primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name:       "char-set?",
			ParamCount: 1,
			Impl:       primCharSetQ,
			Doc:        "Returns #t if obj is a char-set, otherwise #f. (SRFI-14)",
			ParamNames: []string{"obj"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "predicate", "type", "srfi-14"},
		},
		{
			Name:       "char-set-contains?",
			ParamCount: 2,
			Impl:       primCharSetContains,
			Doc:        "Returns #t if char CH is a member of char-set CS, otherwise #f. (SRFI-14)",
			ParamNames: []string{"cs", "ch"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "membership", "contains", "srfi-14"},
		},
		{
			Name:       "char-set-size",
			ParamCount: 1,
			Impl:       primCharSetSize,
			Doc:        "Returns the number of elements in char-set CS as an exact integer. (SRFI-14)",
			ParamNames: []string{"cs"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "size", "cardinality", "srfi-14"},
		},
		{
			// ParamCount: 2, IsVariadic: true → Arg(0) = first (fixed char),
			// Arg(1) = rest list (any additional chars). Per Wile variadic
			// convention: Arg(N-2) is the last fixed param, Arg(N-1) is rest.
			Name:       "%char-set",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       primCharSetCtor,
			Doc:        "Internal helper for SRFI-14 (char-set ...). Use (char-set ...) instead.",
			ParamNames: []string{"first", "rest"},
			Category:   "char-sets",
		},
		{
			Name:       "%empty-char-set",
			ParamCount: 0,
			Impl:       primEmptyCharSet,
			Doc:        "Internal helper: returns the empty char-set. Use (char-set) instead.",
			Category:   "char-sets",
		},
		{
			Name:       "char-set-copy",
			ParamCount: 1,
			Impl:       primCharSetCopy,
			Doc:        "Returns a copy of char-set CS. Wile char-sets are immutable, so the result is char-set= to CS. (SRFI-14)",
			ParamNames: []string{"cs"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "copy", "srfi-14"},
		},
		{
			// ParamCount: 2, IsVariadic: true → Arg(0) = str (fixed),
			// Arg(1) = rest list (optional base char-set, or empty list).
			Name:       "string->char-set",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       primStringToCharSet,
			Doc:        "Returns a char-set containing each char in STR. Optional BASE-CS is unioned into the result. (SRFI-14)",
			ParamNames: []string{"str", "base"},
			Category:   "char-sets",
			Keywords:   []string{"string", "char-set", "convert", "srfi-14"},
		},
		{
			// ParamCount: 2, IsVariadic: true → Arg(0) = lst (fixed),
			// Arg(1) = rest list (optional base char-set, or empty list).
			Name:       "list->char-set",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       primListToCharSet,
			Doc:        "Returns a char-set containing each char in LST. Optional BASE-CS is unioned into the result. (SRFI-14)",
			ParamNames: []string{"lst", "base"},
			Category:   "char-sets",
			Keywords:   []string{"list", "char-set", "convert", "srfi-14"},
		},
		{
			// ParamCount: 3, IsVariadic: true → Arg(0) = lo (fixed),
			// Arg(1) = hi (fixed), Arg(2) = rest list (optional error? and
			// optional base char-set, in that order).
			Name:       "ucs-range->char-set",
			ParamCount: 3,
			IsVariadic: true,
			Impl:       primUcsRangeToCharSet,
			Doc:        "Returns a char-set containing codepoints in the half-open range [LO, HI). Optional ERROR? (default #t) controls out-of-range codepoints at either bound (negative LO, or HI above 0x10FFFF+1): any non-#f value (Scheme-truthy) raises an error, #f silently clips. A malformed range (LO > HI) always raises, regardless of ERROR?. Optional BASE-CS is unioned into the result. (SRFI-14)",
			ParamNames: []string{"lo", "hi", "rest"},
			Category:   "char-sets",
			Keywords:   []string{"ucs", "codepoint", "range", "char-set", "srfi-14"},
		},
		{
			Name:       "char-set->list",
			ParamCount: 1,
			Impl:       primCharSetToList,
			Doc:        "Returns a list of all characters in CS, in codepoint-ascending order. (SRFI-14)",
			ParamNames: []string{"cs"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "list", "convert", "srfi-14"},
		},
		{
			Name:       "char-set->string",
			ParamCount: 1,
			Impl:       primCharSetToString,
			Doc:        "Returns a string of all characters in CS, in codepoint-ascending order. (SRFI-14)",
			ParamNames: []string{"cs"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "string", "convert", "srfi-14"},
		},
		{
			Name:       "char-set-ranges",
			ParamCount: 1,
			Impl:       primCharSetRanges,
			Doc:        "Returns a list of (lo . hi) pairs (inclusive endpoints) for the canonical inversion-list representation of CS. Wile-specific extension to SRFI-14, used internally by iteration procedures.",
			ParamNames: []string{"cs"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "ranges", "wile", "iteration"},
		},
		{
			// ParamCount: 2, IsVariadic: true → Arg(0) = cs1 (fixed),
			// Arg(1) = rest list (any additional char-sets). Per Wile variadic
			// convention: Arg(N-2) is the last fixed param, Arg(N-1) is rest.
			Name:       "char-set=",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       primCharSetEqual,
			Doc:        "Returns #t if all char-sets are equal, otherwise #f. Vacuously true for one argument. (SRFI-14)",
			ParamNames: []string{"cs1", "rest"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "equal", "compare", "srfi-14"},
		},
		{
			// ParamCount: 2, IsVariadic: true → Arg(0) = cs1 (fixed),
			// Arg(1) = rest list (any additional char-sets). Per Wile variadic
			// convention: Arg(N-2) is the last fixed param, Arg(N-1) is rest.
			Name:       "char-set<=",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       primCharSetSubset,
			Doc:        "Returns #t if cs1 ⊆ cs2 ⊆ ... pairwise. Vacuously true for one argument. (SRFI-14)",
			ParamNames: []string{"cs1", "rest"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "subset", "compare", "srfi-14"},
		},
		{
			// ParamCount: 2, IsVariadic: true → Arg(0) = cs1 (fixed),
			// Arg(1) = rest list (any additional char-sets). Per Wile variadic
			// convention: Arg(N-2) is the last fixed param, Arg(N-1) is rest.
			Name:       "%char-set-union",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       makeCharSetFold("char-set-union", unionTwo),
			Doc:        "Internal helper for the n-ary (char-set-union ...). Folds one or more char-sets; the Scheme wrapper supplies the empty-set identity for zero arguments. Use (char-set-union ...) instead.",
			ParamNames: []string{"cs1", "rest"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "union", "srfi-14"},
		},
		{
			// ParamCount: 2, IsVariadic: true → Arg(0) = cs1 (fixed),
			// Arg(1) = rest list (any additional char-sets). Per Wile variadic
			// convention: Arg(N-2) is the last fixed param, Arg(N-1) is rest.
			Name:       "%char-set-intersection",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       makeCharSetFold("char-set-intersection", intersectTwo),
			Doc:        "Internal helper for the n-ary (char-set-intersection ...). Folds one or more char-sets; the Scheme wrapper supplies the full-set identity for zero arguments. Use (char-set-intersection ...) instead.",
			ParamNames: []string{"cs1", "rest"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "intersection", "srfi-14"},
		},
		{
			// ParamCount: 2, IsVariadic: true → Arg(0) = cs1 (fixed),
			// Arg(1) = rest list (any additional char-sets). Per Wile variadic
			// convention: Arg(N-2) is the last fixed param, Arg(N-1) is rest.
			Name:       "char-set-difference",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       makeCharSetFold("char-set-difference", differenceTwo),
			Doc:        "Returns CS1 minus the union of all subsequent char-sets. (SRFI-14)",
			ParamNames: []string{"cs1", "rest"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "difference", "srfi-14"},
		},
		{
			// ParamCount: 2, IsVariadic: true → Arg(0) = cs1 (fixed),
			// Arg(1) = rest list (any additional char-sets). Per Wile variadic
			// convention: Arg(N-2) is the last fixed param, Arg(N-1) is rest.
			Name:       "%char-set-xor",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       makeCharSetFold("char-set-xor", xorTwo),
			Doc:        "Internal helper for the n-ary (char-set-xor ...). Folds one or more char-sets; the Scheme wrapper supplies the empty-set identity for zero arguments. Use (char-set-xor ...) instead.",
			ParamNames: []string{"cs1", "rest"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "xor", "symmetric-difference", "srfi-14"},
		},
		{
			Name:       "char-set-complement",
			ParamCount: 1,
			Impl:       primCharSetComplement,
			Doc:        "Returns the complement of CS within [0, 0x10FFFF]. (SRFI-14)",
			ParamNames: []string{"cs"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "complement", "srfi-14"},
		},
		{
			Name:       "%make-named-charset",
			ParamCount: 1,
			Impl:       primMakeNamedCharSet,
			Doc:        "Internal: returns the named char-set for the given symbol. Used by (srfi 14) to build char-set:letter etc.",
			ParamNames: []string{"name"},
			Category:   "char-sets",
		},
	}, registry.PhaseSetRuntime)
	return nil
}
