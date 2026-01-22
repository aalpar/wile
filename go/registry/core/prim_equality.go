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

package core

import (
	"context"

	"wile/machine"
	"wile/registry/helpers"
	"wile/utils"
	"wile/values"
)

// PrimEqQ implements the eq? predicate for object identity.
// Returns #t if both arguments are the same object (pointer equality).
func PrimEqQ(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)
	// eq? tests for object identity - same pointer or same immediate value
	// Go's == compares pointers by address for reference types
	mc.SetValue(utils.BoolToBoolean(o0 == o1))
	return nil
}

// PrimEqvQ implements the eqv? predicate (R7RS).
// Returns #t if both arguments are operationally equivalent:
// - Same object (pointer equality), OR
// - Both are numbers of the same type with the same value, OR
// - Both are characters with the same value
// Unlike eq?, eqv? treats equivalent numbers/characters as equal even if
// they are different objects. Unlike equal?, eqv? does not recurse into
// pairs, vectors, or strings.
func PrimEqvQ(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)
	mc.SetValue(utils.BoolToBoolean(helpers.Eqv(o0, o1)))
	return nil
}

// PrimEqualQ implements the equal? predicate for structural equality.
// Returns #t if both arguments have the same structure and values.
func PrimEqualQ(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)
	mc.SetValue(utils.BoolToBoolean(values.EqualTo(o0, o1)))
	return nil
}

// PrimNot implements the not primitive.
// Returns #t if the argument is #f, #f otherwise.
func PrimNot(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	// In Scheme, only #f is false; everything else is true
	mc.SetValue(utils.BoolToBoolean(o == values.FalseValue))
	return nil
}
