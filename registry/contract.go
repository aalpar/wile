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

package registry

import (
	"slices"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// BuildValidator creates a contract validation function from a
// PrimitiveSpec's ParamTypes. Returns nil if the spec has no type
// contracts (zero-overhead path for uncontracted primitives).
//
// The returned function is installed on ForeignClosure.validate and runs
// after argument binding but before the implementation. Each argument is
// checked against its declared TypeConstraint via TypeConstraint.Check;
// a mismatch is wrapped with "<primitive>: argument <n>" context so the
// caller can locate the offending position.
//
// For non-variadic primitives, each position 0..ParamCount-1 is checked
// against ParamTypes[i].
//
// For variadic primitives, positions 0..ParamCount-2 are fixed args and
// are checked directly via mc.Arg(i). Position ParamCount-1 holds the
// rest list (a values.Tuple); each rest element is checked against the
// last entry in ParamTypes. When len(ParamTypes) is shorter than
// ParamCount (permitted by validateParamTypes), the last entry is used
// both for the final fixed slot and for the rest elements.
func BuildValidator(spec PrimitiveSpec) machine.ForeignFunction {
	if len(spec.ParamTypes) == 0 {
		return nil
	}
	types := slices.Clone(spec.ParamTypes)
	name := spec.Name
	paramCount := spec.ParamCount
	if spec.IsVariadic {
		return func(mc machine.CallContext) error {
			return validateVariadic(mc, types, name, paramCount)
		}
	}
	return func(mc machine.CallContext) error {
		return validateFixed(mc, types, name)
	}
}

// validateFixed checks each fixed-position arg against its declared
// TypeConstraint. Nil entries are skipped (unspecified).
func validateFixed(mc machine.CallContext, types []values.TypeConstraint, name string) error {
	for i, tc := range types {
		if tc == nil {
			continue
		}
		_, ok, checkErr := tc.Check(mc.Arg(i))
		if !ok {
			return werr.WrapForeignErrorf(checkErr, "%s: argument %d", name, i)
		}
	}
	return nil
}

// validateVariadic checks fixed args at positions 0..paramCount-2 and
// iterates the rest list at mc.Arg(paramCount-1), checking each element
// against the last entry in types.
func validateVariadic(mc machine.CallContext, types []values.TypeConstraint, name string, paramCount int) error {
	fixedCount := paramCount - 1
	for i := 0; i < fixedCount && i < len(types); i++ {
		tc := types[i]
		if tc == nil {
			continue
		}
		_, ok, checkErr := tc.Check(mc.Arg(i))
		if !ok {
			return werr.WrapForeignErrorf(checkErr, "%s: argument %d", name, i)
		}
	}

	restType := types[len(types)-1]
	if restType == nil {
		return nil
	}
	rest, ok := mc.Arg(paramCount - 1).(values.Tuple)
	if !ok {
		return nil
	}

	argIdx := fixedCount
	cursor := rest
	for !cursor.IsEmptyList() {
		_, ok, checkErr := restType.Check(cursor.Car())
		if !ok {
			return werr.WrapForeignErrorf(checkErr, "%s: argument %d", name, argIdx)
		}
		next, nextOk := cursor.Cdr().(values.Tuple)
		if !nextOk {
			break
		}
		cursor = next
		argIdx++
	}
	return nil
}
