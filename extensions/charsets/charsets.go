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
