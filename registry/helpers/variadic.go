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
	"strings"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// VariadicArgs gathers a "fixed args + variadic rest" call into a typed []T.
//
// fixedCount is the ParamCount declared in the PrimitiveSpec; the helper
// expects exactly that many arguments where Arg(fixedCount-1) is the rest
// list. For the common shape ParamCount=2 with IsVariadic=true: Arg(0) is
// fixed, Arg(1) is the rest list, and the returned slice has length 1+|rest|.
//
// All elements (fixed + rest) are type-checked against T. On type mismatch,
// the error names the offending argument's 1-indexed position in the full
// argument vector (fixed positions, then rest positions), matching
// RequireArg's "<name>: argument <N>: expected ... but got ..." format.
//
// Per Wile's variadic convention, fixedCount must be >= 1: ParamCount=0 with
// IsVariadic=true panics in the dispatch layer.
func VariadicArgs[T any](
	mc machine.CallContext,
	fixedCount int,
	sentinel error,
	name string,
) ([]T, error) {
	if fixedCount < 1 {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"%s: VariadicArgs requires fixedCount >= 1, got %d", name, fixedCount)
	}
	fixedArgs := fixedCount - 1
	out := make([]T, 0, fixedArgs+1)

	for i := range fixedArgs {
		v, err := RequireArg[T](mc, i, sentinel, name)
		if err != nil {
			return nil, err
		}
		out = append(out, v)
	}

	rest, ok := mc.Arg(fixedArgs).(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList,
			"%s: rest argument: expected a list but got %T", name, mc.Arg(fixedArgs))
	}
	expectedType := strings.TrimPrefix(sentinel.Error(), "not ")
	err := ForEachList(mc.Context(), rest, name, func(_ context.Context, i int, _ bool, v values.Value) error {
		elem, ok := v.(T)
		if !ok {
			return werr.WrapForeignErrorf(sentinel,
				"%s: argument %d: expected %s but got %T",
				name, fixedArgs+i+1, expectedType, v)
		}
		out = append(out, elem)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}
