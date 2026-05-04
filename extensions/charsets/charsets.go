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
		merged := append(cs.Ranges(), base.Ranges()...)
		cs = values.NewCharSetFromUnsortedRanges(merged)
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

// setValueFromRunesAndBase builds a char-set from runes, optionally unioned
// with a base char-set, and stores it on the machine context.
//
// Phase 1 inlines the base-merge via NewCharSetFromUnsortedRanges (which
// canonicalizes). Phase 2 Task 2.2 will replace this body with unionTwo
// once that helper exists in extensions/charsets/charsets.go.
func setValueFromRunesAndBase(mc machine.CallContext, runes []rune, base *values.CharSet) error {
	cs := values.NewCharSetFromRunes(runes)
	if base != nil {
		merged := append(cs.Ranges(), base.Ranges()...)
		cs = values.NewCharSetFromUnsortedRanges(merged)
	}
	mc.SetValue(cs)
	return nil
}
