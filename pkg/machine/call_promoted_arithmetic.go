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

package machine

import (
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// popTwoNumbers pops two arguments from the eval stack, validates both are
// Number, and updates counters. Returns the two numbers (a, b where a was
// pushed first) or an error if either is not a Number.
func popTwoNumbers(mc *MachineContext, name string) (values.Number, values.Number, error) {
	b, a := mc.evals.Pop2()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += 2
	mc.counters.ForeignCalls++

	an, ok := a.(values.Number)
	if !ok {
		return nil, nil, werr.WrapForeignErrorf(
			werr.ErrNotANumber, "%s: expected number, got %s", name, a.SchemeString())
	}
	bn, ok := b.(values.Number)
	if !ok {
		return nil, nil, werr.WrapForeignErrorf(
			werr.ErrNotANumber, "%s: expected number, got %s", name, b.SchemeString())
	}
	return an, bn, nil
}

// popTwoReals pops two arguments via popTwoNumbers, then additionally requires
// both to be real (R7RS §6.2.6: ordering comparisons reject non-real complex
// arguments). Returns the two reals (a, b where a was pushed first) or an error
// if either is not a Number or is a complex number with a non-zero imaginary
// part.
func popTwoReals(mc *MachineContext, name string) (values.Number, values.Number, error) {
	a, b, err := popTwoNumbers(mc, name)
	if err != nil {
		return nil, nil, err
	}
	if isNonRealComplex(a) || isNonRealComplex(b) {
		return nil, nil, werr.WrapForeignErrorf(
			werr.ErrNotAReal, "%s: requires real arguments", name)
	}
	return a, b, nil
}

// inlineAdd pops two numbers and sets value register to their sum.
func inlineAdd(mc *MachineContext, name string) error {
	a, b, err := popTwoNumbers(mc, name)
	if err != nil {
		return err
	}
	mc.SetValue(a.Add(b))
	return nil
}

// inlineSub pops two numbers and sets value register to a - b.
func inlineSub(mc *MachineContext, name string) error {
	a, b, err := popTwoNumbers(mc, name)
	if err != nil {
		return err
	}
	mc.SetValue(a.Subtract(b))
	return nil
}

// isNonRealComplex returns true if n is a complex number with non-zero
// imaginary part. R7RS §6.2.6: ordering comparisons require real arguments.
// Duplicated from registry/helpers to avoid circular dependency.
func isNonRealComplex(n values.Number) bool {
	switch v := n.(type) {
	case *values.Complex:
		return !v.IsReal()
	case *values.BigComplex:
		return !v.IsReal()
	default:
		return false
	}
}

// The promoted comparison ops derive from values.CompareNumbers, exactly as the
// out-of-line primitives in pkg/registry/core do. They must: the peephole
// promotes a call site into one of these WITHOUT changing what the program
// means, so a promoted `<` that answered differently from the primitive `<`
// would make optimization observable.
//
// NaN needs no special case. It is the kernel's OrderUnordered, which none of
// the five accepts.

// inlineNumLt pops two numbers and sets value register to (< a b).
func inlineNumLt(mc *MachineContext, name string) error {
	a, b, err := popTwoReals(mc, name)
	if err != nil {
		return err
	}
	mc.SetValue(values.BoolToBoolean(values.CompareNumbers(a, b) == values.OrderLess))
	return nil
}

// inlineNumLe pops two numbers and sets value register to (<= a b).
// R7RS + IEEE 754: NaN fails all comparisons.
func inlineNumLe(mc *MachineContext, name string) error {
	a, b, err := popTwoReals(mc, name)
	if err != nil {
		return err
	}
	v := values.CompareNumbers(a, b)
	mc.SetValue(values.BoolToBoolean(v == values.OrderLess || v == values.OrderEqual))
	return nil
}

// inlineNumGt pops two numbers and sets value register to (> a b).
func inlineNumGt(mc *MachineContext, name string) error {
	a, b, err := popTwoReals(mc, name)
	if err != nil {
		return err
	}
	mc.SetValue(values.BoolToBoolean(values.CompareNumbers(a, b) == values.OrderGreater))
	return nil
}

// inlineNumGe pops two numbers and sets value register to (>= a b).
// R7RS + IEEE 754: NaN fails all comparisons.
func inlineNumGe(mc *MachineContext, name string) error {
	a, b, err := popTwoReals(mc, name)
	if err != nil {
		return err
	}
	v := values.CompareNumbers(a, b)
	mc.SetValue(values.BoolToBoolean(v == values.OrderGreater || v == values.OrderEqual))
	return nil
}

// inlineNumEq pops two numbers and sets value register to (= a b).
func inlineNumEq(mc *MachineContext, name string) error {
	a, b, err := popTwoNumbers(mc, name)
	if err != nil {
		return err
	}
	mc.SetValue(values.BoolToBoolean(values.CompareNumbers(a, b) == values.OrderEqual))
	return nil
}

// inlineMul pops two numbers and sets value register to a * b.
func inlineMul(mc *MachineContext, name string) error {
	a, b, err := popTwoNumbers(mc, name)
	if err != nil {
		return err
	}
	mc.SetValue(a.Multiply(b))
	return nil
}

// inlineDiv pops two numbers and sets value register to a / b.
// Returns an error on division by exact zero.
func inlineDiv(mc *MachineContext, name string) error {
	a, b, err := popTwoNumbers(mc, name)
	if err != nil {
		return err
	}
	result, divErr := a.Divide(b)
	if divErr != nil {
		return werr.WrapForeignErrorf(
			divErr, "%s: division error", name)
	}
	mc.SetValue(result)
	return nil
}
