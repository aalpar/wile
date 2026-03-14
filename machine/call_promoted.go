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
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// eqIdentity implements R7RS eq? semantics: pointer equality for all types,
// key equality for symbols. This duplicates registry/helpers.EqIdentity to
// avoid a circular dependency (machine cannot import registry).
func eqIdentity(a, b values.Value) bool {
	sa, ok := a.(*values.Symbol)
	if ok {
		sb, ok2 := b.(*values.Symbol)
		if ok2 {
			return sa.Key == sb.Key
		}
		return false
	}
	return a == b
}

// inlineEq pops two arguments from the eval stack and sets the value register
// to the eq? result. Returns nil on success.
func inlineEq(mc *MachineContext) error {
	b := mc.evals.Pop()
	a := mc.evals.Pop()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += 2
	mc.counters.ForeignCalls++
	mc.SetValue(values.BoolToBoolean(eqIdentity(a, b)))
	return nil
}

// inlineVectorQ pops one argument from the eval stack and sets the value
// register to #t if it is a vector, #f otherwise.
func inlineVectorQ(mc *MachineContext) error {
	o := mc.evals.Pop()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained++
	mc.counters.ForeignCalls++
	_, ok := o.(*values.Vector)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// inlineVectorRef pops two arguments (vector, index) from the eval stack,
// validates them, and sets the value register to the element.
// Returns nil on success or a wrapped error.
func inlineVectorRef(mc *MachineContext) error {
	idx := mc.evals.Pop()
	vec := mc.evals.Pop()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += 2
	mc.counters.ForeignCalls++

	v, ok := vec.(*values.Vector)
	if !ok {
		return applyCallableError(mc, werr.WrapForeignErrorf(
			werr.ErrNotAVector, "vector-ref: expected vector, got %s", vec.SchemeString()))
	}
	i, ok := values.ExactInteger(idx)
	if !ok {
		return applyCallableError(mc, werr.WrapForeignErrorf(
			werr.ErrNotAnInteger, "vector-ref: expected exact integer index, got %s", idx.SchemeString()))
	}
	if i < 0 || i >= int64(v.Length()) {
		return applyCallableError(mc, werr.WrapForeignErrorf(
			werr.ErrIndexOutOfRange, "vector-ref: index %d out of bounds for length %d", i, v.Length()))
	}
	mc.SetValue(v.Get(int(i)))
	return nil
}

// inlineNullQ pops one argument from the eval stack and sets the value
// register to #t if it is the empty list, #f otherwise.
func inlineNullQ(mc *MachineContext) error {
	o := mc.evals.Pop()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained++
	mc.counters.ForeignCalls++
	mc.SetValue(values.BoolToBoolean(values.IsEmptyList(o)))
	return nil
}

// inlinePairQ pops one argument from the eval stack and sets the value
// register to #t if it is a pair, #f otherwise.
func inlinePairQ(mc *MachineContext) error {
	o := mc.evals.Pop()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained++
	mc.counters.ForeignCalls++
	_, ok := o.(*values.Pair)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// inlineCar pops one argument from the eval stack, validates it is a pair,
// and sets the value register to its car. Returns nil on success.
func inlineCar(mc *MachineContext) error {
	o := mc.evals.Pop()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained++
	mc.counters.ForeignCalls++

	p, ok := o.(values.Tuple)
	if !ok || values.IsEmptyList(o) {
		return applyCallableError(mc, werr.WrapForeignErrorf(
			werr.ErrNotAPair, "car: expected pair, got %s", o.SchemeString()))
	}
	mc.SetValue(p.Car())
	return nil
}

// inlineCdr pops one argument from the eval stack, validates it is a pair,
// and sets the value register to its cdr. Returns nil on success.
func inlineCdr(mc *MachineContext) error {
	o := mc.evals.Pop()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained++
	mc.counters.ForeignCalls++

	p, ok := o.(values.Tuple)
	if !ok || values.IsEmptyList(o) {
		return applyCallableError(mc, werr.WrapForeignErrorf(
			werr.ErrNotAPair, "cdr: expected pair, got %s", o.SchemeString()))
	}
	mc.SetValue(p.Cdr())
	return nil
}

// execPromoted runs the common promoted-op dispatch pattern: resolve the
// cached binding, verify it is still the expected ForeignClosure, fall back
// to generic call if not, otherwise execute the inline function and handle
// the tail/non-tail epilogue.
func execPromoted(
	mc *MachineContext,
	instr Instruction,
	name string,
	arity int,
	tail bool,
	fn func(*MachineContext) error,
) (*MachineContext, error) {
	callable := mc.template.cachedBindings[instr.Arg].Value()
	fcls, ok := callable.(*ForeignClosure)
	if !ok || fcls.name != name {
		return callPromotedFallback(mc, callable, tail, arity)
	}
	err := fn(mc)
	if err != nil {
		return nil, err
	}
	if tail {
		return mc.returnImmediate(), nil
	}
	mc.pc++
	return mc, nil
}

// callPromotedFallback handles the case where a promoted primitive's cached
// binding no longer holds a *ForeignClosure at runtime (e.g., reassigned via
// set!). Unlike callForeignCached's fallback, promoted ops have their
// SaveContinuation deleted by the peephole optimizer, so this function uses
// PopN(arity) instead of Drain() to preserve outer stack items, and manually
// saves a continuation for non-tail calls.
func callPromotedFallback(mc *MachineContext, callable values.Value, tail bool, arity int) (*MachineContext, error) {
	vs := mc.evals.PopN(arity)
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += uint64(arity)
	mc.counters.RecordStackDepth(arity)

	if !tail {
		err := mc.SaveContinuation(1)
		if err != nil {
			return nil, err
		}
	}

	result, err := mc.ApplyCallable(callable, vs...)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}
	return result, nil
}

// promotedOpForName returns the non-tail and tail opcodes for a promoted
// primitive, or (OpInvalid, OpInvalid) if the primitive is not promoted.
// Also returns the expected argument count for arity validation.
func promotedOpForName(name string) (nonTail, tail OpCode, arity int) {
	switch name {
	case "eq?":
		return OpEqQ, OpEqQTail, 2
	case "vector?":
		return OpVectorQ, OpVectorQTail, 1
	case "vector-ref":
		return OpVectorRef, OpVectorRefTail, 2
	case "null?":
		return OpNullQ, OpNullQTail, 1
	case "pair?":
		return OpPairQ, OpPairQTail, 1
	case "car":
		return OpCar, OpCarTail, 1
	case "cdr":
		return OpCdr, OpCdrTail, 1
	case "+":
		return OpAdd, OpAddTail, 2
	case "-":
		return OpSub, OpSubTail, 2
	case "<":
		return OpNumLt, OpNumLtTail, 2
	case "<=":
		return OpNumLe, OpNumLeTail, 2
	case ">":
		return OpNumGt, OpNumGtTail, 2
	case ">=":
		return OpNumGe, OpNumGeTail, 2
	case "=":
		return OpNumEq, OpNumEqTail, 2
	default:
		return OpInvalid, OpInvalid, 0
	}
}
