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
