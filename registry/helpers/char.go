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

package helpers

import (
	"context"
	"errors"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// CharCompareVariadic is a helper for variadic character comparison primitives.
// It extracts characters from the variadic args and applies the comparator pairwise.
func CharCompareVariadic(mc machine.CallContext, name string, cmp func(a, b rune) bool) error {
	return CompareVariadic(mc, name, werr.ErrNotACharacter,
		func(c *values.Character) rune {
			return c.Value
		}, cmp)
}

// CompareVariadic implements pairwise chain comparison over a homogeneous
// variadic list with short-circuit on first failed pair.
//
// Type-checks and compares each rest element as it walks the list — does
// NOT pre-materialize all args. This preserves the historical behavior:
// `(char<? #\b #\a not-a-char)` returns #f via cmp(#\b, #\a)→false without
// ever inspecting the third (ill-typed) argument. Going through VariadicArgs
// would type-check all elements first, raising a type error instead.
//
// Used for comparison primitives like char<?, string<?, etc. where the call
// shape is `(op a b c ...)` with all args of the same type. Short-circuit
// on cmp failure uses werr.ErrCannotCompare as a non-error signal, matching
// the NumericChainCompare pattern. The expected-type phrase is read from
// the sentinel via TypeName().
func CompareVariadic[T values.Value, V any](
	mc machine.CallContext,
	name string,
	sentinel error,
	extract func(T) V,
	cmp func(V, V) bool,
) error {
	typeName := typeNameFromSentinel(sentinel)
	first, err := RequireArg[T](mc, 0, sentinel, name)
	if err != nil {
		return err
	}
	prev := extract(first)
	rest, err := RequireTuple(mc, 1, name)
	if err != nil {
		return err
	}
	err = ForEachList(mc.Context(), rest, name, func(_ context.Context, i int, _ bool, v values.Value) error {
		elem, ok := v.(T)
		if !ok {
			return werr.WrapForeignErrorf(sentinel,
				"%s: argument %d: expected %s but got %T",
				name, i+2, typeName, v)
		}
		current := extract(elem)
		if !cmp(prev, current) {
			return werr.ErrCannotCompare
		}
		prev = current
		return nil
	})
	if errors.Is(err, werr.ErrCannotCompare) {
		mc.SetValue(values.FalseValue)
		return nil
	}
	if err != nil {
		return err
	}
	mc.SetValue(values.TrueValue)
	return nil
}
