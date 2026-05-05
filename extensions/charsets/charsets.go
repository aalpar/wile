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
	"context"
	"sync"
	"unicode"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

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
			Doc:        "Returns a char-set containing codepoints in the half-open range [LO, HI). Optional ERROR? (default #t) controls handling of codepoints exceeding 0x10FFFF: any non-#f value (Scheme-truthy) raises an error, #f silently clips. Optional BASE-CS is unioned into the result. (SRFI-14)",
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
			Name:       "char-set-union",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       primCharSetUnion,
			Doc:        "Returns the union of all char-sets. (SRFI-14)",
			ParamNames: []string{"cs1", "rest"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "union", "srfi-14"},
		},
		{
			// ParamCount: 2, IsVariadic: true → Arg(0) = cs1 (fixed),
			// Arg(1) = rest list (any additional char-sets). Per Wile variadic
			// convention: Arg(N-2) is the last fixed param, Arg(N-1) is rest.
			Name:       "char-set-intersection",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       primCharSetIntersection,
			Doc:        "Returns the intersection of all char-sets. (SRFI-14)",
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
			Impl:       primCharSetDifference,
			Doc:        "Returns CS1 minus the union of all subsequent char-sets. (SRFI-14)",
			ParamNames: []string{"cs1", "rest"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "difference", "srfi-14"},
		},
		{
			// ParamCount: 2, IsVariadic: true → Arg(0) = cs1 (fixed),
			// Arg(1) = rest list (any additional char-sets). Per Wile variadic
			// convention: Arg(N-2) is the last fixed param, Arg(N-1) is rest.
			Name:       "char-set-xor",
			ParamCount: 2,
			IsVariadic: true,
			Impl:       primCharSetXor,
			Doc:        "Returns the symmetric difference of all char-sets. (SRFI-14)",
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
	}, registry.PhaseRuntime|registry.PhaseExpand)
	return nil
}

func primCharSetQ(mc machine.CallContext) error {
	_, ok := mc.Arg(0).(*values.CharSet)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

func primCharSetContains(mc machine.CallContext) error {
	cs, ok := mc.Arg(0).(*values.CharSet)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotACharSet,
			"char-set-contains?: argument 1: expected char-set, got %T", mc.Arg(0))
	}
	ch, ok := mc.Arg(1).(*values.Character)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotACharacter,
			"char-set-contains?: argument 2: expected char, got %T", mc.Arg(1))
	}
	mc.SetValue(values.BoolToBoolean(cs.Contains(ch.Value)))
	return nil
}

func primCharSetSize(mc machine.CallContext) error {
	cs, ok := mc.Arg(0).(*values.CharSet)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotACharSet,
			"char-set-size: argument 1: expected char-set, got %T", mc.Arg(0))
	}
	mc.SetValue(values.NewInteger(int64(cs.Size())))
	return nil
}

func primCharSetCtor(mc machine.CallContext) error {
	first, ok := mc.Arg(0).(*values.Character)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotACharacter,
			"char-set: argument 1: expected char, got %T", mc.Arg(0))
	}
	runes := []rune{first.Value}

	// mc.Arg(1) is the variadic-rest list (always a Tuple — Pair or
	// EmptyList — never nil per Wile variadic convention for ParamCount=2).
	rest, ok := mc.Arg(1).(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList,
			"char-set: rest argument: expected list of chars, got %T", mc.Arg(1))
	}
	var iterErr error
	_, _ = rest.ForEach(mc.Context(), func(_ context.Context, i int, _ bool, v values.Value) error {
		ch, isChar := v.(*values.Character)
		if !isChar {
			iterErr = werr.WrapForeignErrorf(werr.ErrNotACharacter,
				"char-set: argument %d: expected char, got %T", i+2, v)
			return iterErr
		}
		runes = append(runes, ch.Value)
		return nil
	})
	if iterErr != nil {
		return iterErr
	}

	mc.SetValue(values.NewCharSetFromRunes(runes))
	return nil
}

func primEmptyCharSet(mc machine.CallContext) error {
	mc.SetValue(values.NewCharSetFromRanges(nil))
	return nil
}

func primCharSetCopy(mc machine.CallContext) error {
	cs, ok := mc.Arg(0).(*values.CharSet)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotACharSet,
			"char-set-copy: argument 1: expected char-set, got %T", mc.Arg(0))
	}
	// CharSet is immutable; copy is identity at the Go level. Returning
	// a fresh wrapper is unnecessary, but harmless if a future change adds
	// hidden state. Identity is the cheapest correct answer.
	mc.SetValue(cs)
	return nil
}

func primStringToCharSet(mc machine.CallContext) error {
	str, ok := mc.Arg(0).(*values.String)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAString,
			"string->char-set: argument 1: expected string, got %T", mc.Arg(0))
	}
	runes := []rune(str.Value)
	base, err := optionalBaseCharSet("string->char-set", mc.Arg(1))
	if err != nil {
		return err
	}
	return setValueFromRunesAndBase(mc, runes, base)
}

func primListToCharSet(mc machine.CallContext) error {
	lst, ok := mc.Arg(0).(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList,
			"list->char-set: argument 1: expected list, got %T", mc.Arg(0))
	}
	var runes []rune
	var iterErr error
	_, _ = lst.ForEach(mc.Context(), func(_ context.Context, i int, _ bool, v values.Value) error {
		ch, isChar := v.(*values.Character)
		if !isChar {
			iterErr = werr.WrapForeignErrorf(werr.ErrNotACharacter,
				"list->char-set: list element %d: expected char, got %T", i, v)
			return iterErr
		}
		runes = append(runes, ch.Value)
		return nil
	})
	if iterErr != nil {
		return iterErr
	}
	base, err := optionalBaseCharSet("list->char-set", mc.Arg(1))
	if err != nil {
		return err
	}
	return setValueFromRunesAndBase(mc, runes, base)
}

// optionalBaseCharSet extracts an optional base char-set from a variadic-rest
// argument. Returns (nil, nil) for no base; (cs, nil) for a valid base;
// (nil, err) on type mismatch.
func optionalBaseCharSet(site string, restArg values.Value) (*values.CharSet, error) {
	rest, ok := restArg.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList,
			"%s: rest argument: expected list, got %T", site, restArg)
	}
	if rest.IsEmptyList() {
		return nil, nil
	}
	first := rest.Car()
	base, isCs := first.(*values.CharSet)
	if !isCs {
		return nil, werr.WrapForeignErrorf(werr.ErrNotACharSet,
			"%s: optional base argument: expected char-set, got %T", site, first)
	}
	return base, nil
}

// exactInt64 extracts an int64 from any exact integer Number value.
func exactInt64(v values.Value) (int64, bool) {
	return values.ExactInteger(v)
}

func primUcsRangeToCharSet(mc machine.CallContext) error {
	lo, ok := exactInt64(mc.Arg(0))
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnInteger,
			"ucs-range->char-set: argument 1: expected exact integer, got %T", mc.Arg(0))
	}
	hi, ok := exactInt64(mc.Arg(1))
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnInteger,
			"ucs-range->char-set: argument 2: expected exact integer, got %T", mc.Arg(1))
	}
	if lo > hi {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"ucs-range->char-set: lo (%d) exceeds hi (%d)", lo, hi)
	}

	// Parse optional rest: (error? base-cs).
	//
	// Per CLAUDE.md "ParamCount: N, IsVariadic: true → Arg(N-1) = rest list"
	// the rest is always a Tuple (Pair or EmptyList), never nil.
	errorFlag := true
	var base *values.CharSet
	rest, ok := mc.Arg(2).(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList,
			"ucs-range->char-set: rest argument: expected list, got %T", mc.Arg(2))
	}
	if !rest.IsEmptyList() {
		// Scheme truthiness: only #f is false; any other value (including
		// symbols, integers, strings) is truthy. SRFI-14 spec calls error?
		// a "boolean" but doesn't pin down rejection behavior for non-booleans;
		// following Scheme convention here.
		errorFlag = rest.Car() != values.FalseValue
		cdr := rest.Cdr()
		cdrTuple, isTuple := cdr.(values.Tuple)
		if isTuple && !cdrTuple.IsEmptyList() {
			cs, isCs := cdrTuple.Car().(*values.CharSet)
			if !isCs {
				return werr.WrapForeignErrorf(werr.ErrNotACharSet,
					"ucs-range->char-set: optional base argument: expected char-set, got %T", cdrTuple.Car())
			}
			base = cs
		}
	}

	// Validate or clip the lower bound.
	lowerInclusive := lo
	if lowerInclusive < 0 {
		if errorFlag {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"ucs-range->char-set: codepoint %d is negative", lo)
		}
		lowerInclusive = 0
	}

	// Validate or clip the upper bound.
	upperExclusive := hi
	if upperExclusive > int64(values.MaxCodepoint)+1 {
		if errorFlag {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"ucs-range->char-set: codepoint %d exceeds 0x10FFFF", hi-1)
		}
		upperExclusive = int64(values.MaxCodepoint) + 1
	}

	// Half-open [lo, hi) → inclusive [lowerInclusive, upperExclusive-1].
	var cs *values.CharSet
	if lowerInclusive >= upperExclusive {
		cs = values.NewCharSetFromRanges(nil)
	} else {
		cs = values.NewCharSetFromRanges([]values.CharSetRange{
			{Lo: rune(lowerInclusive), Hi: rune(upperExclusive - 1)},
		})
	}
	if base != nil {
		cs = unionTwo(cs, base)
	}
	mc.SetValue(cs)
	return nil
}

func primCharSetToList(mc machine.CallContext) error {
	cs, ok := mc.Arg(0).(*values.CharSet)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotACharSet,
			"char-set->list: argument 1: expected char-set, got %T", mc.Arg(0))
	}
	chars := make([]values.Value, 0, cs.Size())
	for _, r := range cs.Ranges() {
		for c := r.Lo; c <= r.Hi; c++ {
			chars = append(chars, values.NewCharacter(c))
		}
	}
	mc.SetValue(values.List(chars...))
	return nil
}

func primCharSetToString(mc machine.CallContext) error {
	cs, ok := mc.Arg(0).(*values.CharSet)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotACharSet,
			"char-set->string: argument 1: expected char-set, got %T", mc.Arg(0))
	}
	runes := make([]rune, 0, cs.Size())
	for _, r := range cs.Ranges() {
		for c := r.Lo; c <= r.Hi; c++ {
			runes = append(runes, c)
		}
	}
	mc.SetValue(values.NewString(string(runes)))
	return nil
}

func primCharSetRanges(mc machine.CallContext) error {
	cs, ok := mc.Arg(0).(*values.CharSet)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotACharSet,
			"char-set-ranges: argument 1: expected char-set, got %T", mc.Arg(0))
	}
	rs := cs.Ranges()
	pairs := make([]values.Value, len(rs))
	for i, r := range rs {
		pairs[i] = values.NewCons(values.NewInteger(int64(r.Lo)), values.NewInteger(int64(r.Hi)))
	}
	mc.SetValue(values.List(pairs...))
	return nil
}

// setValueFromRunesAndBase builds a char-set from runes, optionally unioned
// with a base char-set, and stores it on the machine context.
func setValueFromRunesAndBase(mc machine.CallContext, runes []rune, base *values.CharSet) error {
	cs := values.NewCharSetFromRunes(runes)
	if base != nil {
		cs = unionTwo(cs, base)
	}
	mc.SetValue(cs)
	return nil
}

// charSetVariadicArgs collects the first arg + variadic rest into a single slice
// of *CharSet, with type-error wrapping. Returns ([]cs, nil) on success.
func charSetVariadicArgs(site string, mc machine.CallContext) ([]*values.CharSet, error) {
	first, ok := mc.Arg(0).(*values.CharSet)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotACharSet,
			"%s: argument 1: expected char-set, got %T", site, mc.Arg(0))
	}
	out := []*values.CharSet{first}
	rest, ok := mc.Arg(1).(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList,
			"%s: rest argument: expected list, got %T", site, mc.Arg(1))
	}
	var iterErr error
	_, _ = rest.ForEach(mc.Context(), func(_ context.Context, i int, _ bool, v values.Value) error {
		cs, isCs := v.(*values.CharSet)
		if !isCs {
			iterErr = werr.WrapForeignErrorf(werr.ErrNotACharSet,
				"%s: argument %d: expected char-set, got %T", site, i+2, v)
			return iterErr
		}
		out = append(out, cs)
		return nil
	})
	if iterErr != nil {
		return nil, iterErr
	}
	return out, nil
}

func primCharSetEqual(mc machine.CallContext) error {
	sets, err := charSetVariadicArgs("char-set=", mc)
	if err != nil {
		return err
	}
	for i := 1; i < len(sets); i++ {
		if !sets[0].EqualTo(sets[i]) {
			mc.SetValue(values.FalseValue)
			return nil
		}
	}
	mc.SetValue(values.TrueValue)
	return nil
}

func primCharSetSubset(mc machine.CallContext) error {
	sets, err := charSetVariadicArgs("char-set<=", mc)
	if err != nil {
		return err
	}
	for i := 1; i < len(sets); i++ {
		if !isSubset(sets[i-1], sets[i]) {
			mc.SetValue(values.FalseValue)
			return nil
		}
	}
	mc.SetValue(values.TrueValue)
	return nil
}

func primCharSetUnion(mc machine.CallContext) error {
	sets, err := charSetVariadicArgs("char-set-union", mc)
	if err != nil {
		return err
	}
	out := sets[0]
	for i := 1; i < len(sets); i++ {
		out = unionTwo(out, sets[i])
	}
	mc.SetValue(out)
	return nil
}

func primCharSetIntersection(mc machine.CallContext) error {
	sets, err := charSetVariadicArgs("char-set-intersection", mc)
	if err != nil {
		return err
	}
	out := sets[0]
	for i := 1; i < len(sets); i++ {
		out = intersectTwo(out, sets[i])
	}
	mc.SetValue(out)
	return nil
}

func primCharSetDifference(mc machine.CallContext) error {
	sets, err := charSetVariadicArgs("char-set-difference", mc)
	if err != nil {
		return err
	}
	out := sets[0]
	for i := 1; i < len(sets); i++ {
		out = differenceTwo(out, sets[i])
	}
	mc.SetValue(out)
	return nil
}

func primCharSetXor(mc machine.CallContext) error {
	sets, err := charSetVariadicArgs("char-set-xor", mc)
	if err != nil {
		return err
	}
	out := sets[0]
	for i := 1; i < len(sets); i++ {
		out = xorTwo(out, sets[i])
	}
	mc.SetValue(out)
	return nil
}

func primCharSetComplement(mc machine.CallContext) error {
	cs, ok := mc.Arg(0).(*values.CharSet)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotACharSet,
			"char-set-complement: argument 1: expected char-set, got %T", mc.Arg(0))
	}
	mc.SetValue(complementOne(cs))
	return nil
}

// unionTwo computes a ∪ b via NewCharSetFromUnsortedRanges, which canonicalizes
// (sorts + merges adjacent + drops invalid). Correct because the result is
// unconditionally re-canonicalized.
func unionTwo(a, b *values.CharSet) *values.CharSet {
	return values.NewCharSetFromUnsortedRanges(append(a.Ranges(), b.Ranges()...))
}

// intersectTwo computes a ∩ b via linear merge over sorted ranges.
func intersectTwo(a, b *values.CharSet) *values.CharSet {
	ar, br := a.Ranges(), b.Ranges()
	var out []values.CharSetRange
	i, j := 0, 0
	for i < len(ar) && j < len(br) {
		lo := max(ar[i].Lo, br[j].Lo)
		hi := min(ar[i].Hi, br[j].Hi)
		if lo <= hi {
			out = append(out, values.CharSetRange{Lo: lo, Hi: hi})
		}
		if ar[i].Hi < br[j].Hi {
			i++
		} else {
			j++
		}
	}
	return values.NewCharSetFromRanges(out) // already canonical
}

// differenceTwo computes a \ b via linear merge over sorted ranges.
func differenceTwo(a, b *values.CharSet) *values.CharSet {
	ar, br := a.Ranges(), b.Ranges()
	var out []values.CharSetRange
	j := 0
	for _, r := range ar {
		cur := r
		// Advance past b-ranges that end before cur starts.
		for j < len(br) && br[j].Hi < cur.Lo {
			j++
		}
		// Subtract each overlapping b-range from cur.
		for j < len(br) && br[j].Lo <= cur.Hi {
			if br[j].Lo > cur.Lo {
				out = append(out, values.CharSetRange{Lo: cur.Lo, Hi: br[j].Lo - 1})
			}
			if br[j].Hi >= cur.Hi {
				cur.Lo = cur.Hi + 1 // mark as fully consumed
				break
			}
			cur.Lo = br[j].Hi + 1
			j++
		}
		if cur.Lo <= cur.Hi {
			out = append(out, cur)
		}
	}
	return values.NewCharSetFromRanges(out)
}

// xorTwo computes a △ b = (a ∪ b) \ (a ∩ b).
func xorTwo(a, b *values.CharSet) *values.CharSet {
	return differenceTwo(unionTwo(a, b), intersectTwo(a, b))
}

// complementOne computes [0, MaxCodepoint] \ cs.
func complementOne(cs *values.CharSet) *values.CharSet {
	full := values.NewCharSetFromRanges([]values.CharSetRange{{Lo: 0, Hi: values.MaxCodepoint}})
	return differenceTwo(full, cs)
}

// isSubset reports whether every codepoint of a is also in b. Linear merge
// over canonical inversion lists: O(n_a + n_b).
func isSubset(a, b *values.CharSet) bool {
	ar := a.Ranges()
	br := b.Ranges()
	j := 0
	for _, ra := range ar {
		// Advance past b-ranges that end before this a-range starts.
		for j < len(br) && br[j].Hi < ra.Lo {
			j++
		}
		if j >= len(br) || br[j].Lo > ra.Lo || br[j].Hi < ra.Hi {
			return false
		}
	}
	return true
}

// namedCharSets is a process-global cache. Built lazily on first request,
// then returned by pointer (eq? from Scheme). Per design §7.
var (
	namedCharSetsMu sync.Mutex
	namedCharSets   = map[string]*values.CharSet{}
)

// makeNamedCharSet returns the cached or freshly-built named char-set for the
// given SRFI-14 name. Symbol-dispatched factory hitting Go's unicode.RangeTable.
func makeNamedCharSet(name string) (*values.CharSet, error) {
	namedCharSetsMu.Lock()
	defer namedCharSetsMu.Unlock()

	cs, ok := namedCharSets[name]
	if ok {
		return cs, nil
	}

	switch name {
	case "letter":
		cs = rangeTableToCharSet(unicode.L)
	case "lower-case":
		cs = rangeTableToCharSet(unicode.Ll)
	case "upper-case":
		cs = rangeTableToCharSet(unicode.Lu)
	case "title-case":
		cs = rangeTableToCharSet(unicode.Lt)
	case "digit":
		cs = rangeTableToCharSet(unicode.Nd)
	case "letter+digit":
		cs = unionTwo(rangeTableToCharSet(unicode.L), rangeTableToCharSet(unicode.Nd))
	case "graphic":
		cs = rangeListToCharSet(unicode.GraphicRanges)
	case "printing":
		cs = rangeListToCharSet(unicode.PrintRanges)
	case "whitespace":
		cs = rangeTableToCharSet(unicode.White_Space)
	case "iso-control":
		cs = rangeTableToCharSet(unicode.Cc)
	case "punctuation":
		cs = rangeTableToCharSet(unicode.P)
	case "symbol":
		cs = rangeTableToCharSet(unicode.S)
	case "hex-digit":
		// ASCII-only per SRFI-14: 0-9, A-F, a-f
		cs = values.NewCharSetFromRanges([]values.CharSetRange{
			{Lo: '0', Hi: '9'}, {Lo: 'A', Hi: 'F'}, {Lo: 'a', Hi: 'f'},
		})
	case "blank":
		// Tab + space + Unicode Zs category
		zs := rangeTableToCharSet(unicode.Zs)
		cs = unionTwo(values.NewCharSetFromRanges([]values.CharSetRange{
			{Lo: '\t', Hi: '\t'}, {Lo: ' ', Hi: ' '},
		}), zs)
	default:
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"%%make-named-charset: unknown named char-set: %s", name)
	}
	namedCharSets[name] = cs
	return cs, nil
}

// rangeTableToCharSet converts a *unicode.RangeTable into a CharSet.
// Handles both Range16 (BMP, codepoints ≤ 0xFFFF) and Range32 (supplementary
// planes). Stride > 1 means the range covers every Stride-th codepoint;
// expand into per-stride unit ranges.
//
// NOTE: the loop variable must be promoted to uint32 for R16 ranges. Using
// uint16 directly (the element type of Range16 fields) overflows when
// hi + stride > 65535, producing wraparound values that contaminate the set.
func rangeTableToCharSet(t *unicode.RangeTable) *values.CharSet {
	var rs []values.CharSetRange
	for _, r := range t.R16 {
		if r.Stride == 1 {
			rs = append(rs, values.CharSetRange{Lo: rune(r.Lo), Hi: rune(r.Hi)})
			continue
		}
		// Promote to uint32 to avoid uint16 wraparound when lo + stride > 65535.
		for cp := uint32(r.Lo); cp <= uint32(r.Hi); cp += uint32(r.Stride) {
			rs = append(rs, values.CharSetRange{Lo: rune(cp), Hi: rune(cp)})
		}
	}
	for _, r := range t.R32 {
		if r.Stride == 1 {
			rs = append(rs, values.CharSetRange{Lo: rune(r.Lo), Hi: rune(r.Hi)})
			continue
		}
		for cp := r.Lo; cp <= r.Hi; cp += r.Stride {
			rs = append(rs, values.CharSetRange{Lo: rune(cp), Hi: rune(cp)})
		}
	}
	return values.NewCharSetFromUnsortedRanges(rs)
}

// rangeListToCharSet unions multiple RangeTables (for unicode.GraphicRanges,
// PrintRanges which are []*RangeTable).
func rangeListToCharSet(tables []*unicode.RangeTable) *values.CharSet {
	out := values.NewCharSetFromRanges(nil)
	for _, t := range tables {
		out = unionTwo(out, rangeTableToCharSet(t))
	}
	return out
}

// primMakeNamedCharSet is the FFI dispatcher for %make-named-charset.
func primMakeNamedCharSet(mc machine.CallContext) error {
	sym, ok := mc.Arg(0).(*values.Symbol)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotASymbol,
			"%%make-named-charset: argument 1: expected symbol, got %T", mc.Arg(0))
	}
	cs, err := makeNamedCharSet(sym.Key)
	if err != nil {
		return err
	}
	mc.SetValue(cs)
	return nil
}
