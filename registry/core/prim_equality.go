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
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// makeBinaryPredicate returns a 2-argument primitive that applies cmp to its
// arguments and sets the boolean result. eq?, eqv?, and equal? (R7RS §6.1)
// share this factory — they differ only in the comparator, and the
// argument-fetch/BoolToBoolean/SetValue prologue is invariant across them.
func makeBinaryPredicate(cmp func(a, b values.Value) bool) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		mc.SetValue(values.BoolToBoolean(cmp(mc.Arg(0), mc.Arg(1))))
		return nil
	}
}

// PrimEqQ implements the eq? predicate (R7RS §6.1).
// Returns #t if both arguments are identical: pointer equality for most types,
// string key comparison for symbols (R7RS §6.5).
var PrimEqQ = makeBinaryPredicate(helpers.EqIdentity)

// PrimEqvQ implements the eqv? predicate (R7RS).
// Returns #t if both arguments are operationally equivalent:
// - Same object (pointer equality), OR
// - Both are numbers of the same type with the same value, OR
// - Both are characters with the same value
// Unlike eq?, eqv? treats equivalent numbers/characters as equal even if
// they are different objects. Unlike equal?, eqv? does not recurse into
// pairs, vectors, or strings.
var PrimEqvQ = makeBinaryPredicate(helpers.Eqv)

// PrimEqualQ implements the equal? predicate for structural equality.
// Returns #t if both arguments have the same structure and values.
var PrimEqualQ = makeBinaryPredicate(values.EqualTo)
