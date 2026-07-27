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

// ADDING A NEW PROMOTED OP
//
// Promoted ops inline hot primitives directly in the VM dispatch loop,
// bypassing arity check, arg binding, and indirect function call.
// Each promoted op has a non-tail and tail variant.
//
// The 34 switch cases in Run() are deliberately hand-unrolled: Go compiles
// them to a jump table. Replacing them with a table lookup in the default
// branch was benchmarked and rejected (~1.5% geo mean regression). See
// memory/2026-04-05-structural-reduction.local.md.
//
// Passing each op's facts as a *promotedOp instead of as call-site immediates
// costs a further ~0.33% geo mean (two interleaved A/B passes, min-of-8,
// agreeing to 0.01%; mostly op.fn becoming an indirect call the compiler can no
// longer devirtualize). That cost was measured and accepted, in exchange for
// stating each op's name and arity exactly once. Do not re-expand the
// descriptor back into immediates without re-running that A/B — and note that
// sequential before/after cannot see an effect this small: two runs of the SAME
// binary drift ~1.2% geo mean, 14/16 benchmarks in the same direction. Only
// interleaving the two binaries within each run cancels that drift.
//
// Edit sites (3 files):
//  1. opcode.go           — add OpXxx + OpXxxTail constants inside the promoted
//     range (OpEqQ..OpDivTail); add two opcodeTable entries with
//     operandKind: OperandCachedBinding
//  2. call_promoted.go    — implement inlineXxx(mc *MachineContext, name string) error;
//     add a promotedOp descriptor and list it in promotedOps (or
//     call_promoted_arithmetic.go for numeric ops). The Scheme name, arity, and
//     both opcodes are stated once, in the descriptor — do not restate the name
//     in the inline function's diagnostics; it arrives as the name parameter.
//  3. machine_context.go  — add two case branches in Run() (non-tail + tail),
//     each calling execPromoted(mc, instr, &promotedXxx, tail)
//
// No changes needed in native_template.go or disassemble.go — both use
// opcodeTable[op].operandKind metadata to handle OperandCachedBinding generically.
// The peephole optimizer (peephole.go) also needs no changes — it uses
// promotedOpForName() to discover promoted ops generically.
package machine

import (
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// promotedOp describes one promoted primitive. Every fact about the op is
// stated here exactly once: the Scheme name the cached binding is guarded
// against (also the RecordCall key and the prefix fn's diagnostics use), the
// argument count the fallback path pops, the two opcodes the peephole optimizer
// emits, and the inline implementation. Run()'s switch and promotedOpForName
// both read the descriptor rather than restating any of it.
type promotedOp struct {
	name    string
	arity   int
	nonTail OpCode
	tail    OpCode
	fn      func(*MachineContext, string) error
}

// One descriptor per promoted primitive. Run()'s case branches reference these
// by address; the shared promotedOps list below drives name lookup.
var (
	promotedEqQ       = promotedOp{name: "eq?", arity: 2, nonTail: OpEqQ, tail: OpEqQTail, fn: inlineEq}
	promotedVectorQ   = promotedOp{name: "vector?", arity: 1, nonTail: OpVectorQ, tail: OpVectorQTail, fn: inlineVectorQ}
	promotedVectorRef = promotedOp{name: "vector-ref", arity: 2, nonTail: OpVectorRef, tail: OpVectorRefTail, fn: inlineVectorRef}
	promotedNullQ     = promotedOp{name: "null?", arity: 1, nonTail: OpNullQ, tail: OpNullQTail, fn: inlineNullQ}
	promotedPairQ     = promotedOp{name: "pair?", arity: 1, nonTail: OpPairQ, tail: OpPairQTail, fn: inlinePairQ}
	promotedCar       = promotedOp{name: "car", arity: 1, nonTail: OpCar, tail: OpCarTail, fn: inlineCar}
	promotedCdr       = promotedOp{name: "cdr", arity: 1, nonTail: OpCdr, tail: OpCdrTail, fn: inlineCdr}
	promotedCons      = promotedOp{name: "cons", arity: 2, nonTail: OpCons, tail: OpConsTail, fn: inlineCons}
	promotedAdd       = promotedOp{name: "+", arity: 2, nonTail: OpAdd, tail: OpAddTail, fn: inlineAdd}
	promotedSub       = promotedOp{name: "-", arity: 2, nonTail: OpSub, tail: OpSubTail, fn: inlineSub}
	promotedMul       = promotedOp{name: "*", arity: 2, nonTail: OpMul, tail: OpMulTail, fn: inlineMul}
	promotedDiv       = promotedOp{name: "/", arity: 2, nonTail: OpDiv, tail: OpDivTail, fn: inlineDiv}
	promotedNumLt     = promotedOp{name: "<", arity: 2, nonTail: OpNumLt, tail: OpNumLtTail, fn: inlineNumLt}
	promotedNumLe     = promotedOp{name: "<=", arity: 2, nonTail: OpNumLe, tail: OpNumLeTail, fn: inlineNumLe}
	promotedNumGt     = promotedOp{name: ">", arity: 2, nonTail: OpNumGt, tail: OpNumGtTail, fn: inlineNumGt}
	promotedNumGe     = promotedOp{name: ">=", arity: 2, nonTail: OpNumGe, tail: OpNumGeTail, fn: inlineNumGe}
	promotedNumEq     = promotedOp{name: "=", arity: 2, nonTail: OpNumEq, tail: OpNumEqTail, fn: inlineNumEq}
)

// promotedOps is the registry of promoted primitives. Listing a descriptor here
// is what makes the peephole optimizer aware of it (via promotedOpForName);
// TestPromotedOpsCoverPromotedOpcodes pins that the list covers every opcode in
// the promoted range, so a descriptor left off the list fails the build's tests
// rather than silently disabling promotion for that primitive.
var promotedOps = []*promotedOp{
	&promotedEqQ, &promotedVectorQ, &promotedVectorRef, &promotedNullQ,
	&promotedPairQ, &promotedCar, &promotedCdr, &promotedCons,
	&promotedAdd, &promotedSub, &promotedMul, &promotedDiv,
	&promotedNumLt, &promotedNumLe, &promotedNumGt, &promotedNumGe,
	&promotedNumEq,
}

// promotedByName indexes promotedOps by Scheme name. Built once at init; read
// only on the compile path (promotedOpForName), never in the VM dispatch loop.
var promotedByName = buildPromotedByName()

// buildPromotedByName indexes promotedOps by Scheme name.
func buildPromotedByName() map[string]*promotedOp {
	q := make(map[string]*promotedOp, len(promotedOps))
	for _, op := range promotedOps {
		q[op.name] = op
	}
	return q
}

// inlineEq pops two arguments from the eval stack and sets the value register
// to the eq? result. Returns nil on success. Cannot fail, so it ignores the
// primitive name execPromoted threads through for diagnostics.
func inlineEq(mc *MachineContext, _ string) error {
	b, a := mc.evals.Pop2()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += 2
	mc.counters.ForeignCalls++
	mc.SetValue(values.BoolToBoolean(values.EqIdentity(a, b)))
	return nil
}

// inlineVectorQ pops one argument from the eval stack and sets the value
// register to #t if it is a vector, #f otherwise. Cannot fail, so it ignores
// the primitive name execPromoted threads through for diagnostics.
func inlineVectorQ(mc *MachineContext, _ string) error {
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
func inlineVectorRef(mc *MachineContext, name string) error {
	idx, vec := mc.evals.Pop2()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += 2
	mc.counters.ForeignCalls++

	v, ok := vec.(*values.Vector)
	if !ok {
		return werr.WrapForeignErrorf(
			werr.ErrNotAVector, "%s: expected vector, got %s", name, vec.SchemeString())
	}
	i, ok := values.ExactInteger(idx)
	if !ok {
		return werr.WrapForeignErrorf(
			werr.ErrNotAnInteger, "%s: expected exact integer index, got %s", name, idx.SchemeString())
	}
	if i < 0 || i >= int64(v.Length()) {
		return werr.WrapForeignErrorf(
			werr.ErrIndexOutOfRange, "%s: index %d out of bounds for length %d", name, i, v.Length())
	}
	mc.SetValue(v.Get(int(i)))
	return nil
}

// inlineNullQ pops one argument from the eval stack and sets the value
// register to #t if it is the empty list, #f otherwise. Cannot fail, so it
// ignores the primitive name execPromoted threads through for diagnostics.
func inlineNullQ(mc *MachineContext, _ string) error {
	o := mc.evals.Pop()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained++
	mc.counters.ForeignCalls++
	mc.SetValue(values.BoolToBoolean(values.IsEmptyList(o)))
	return nil
}

// inlinePairQ pops one argument from the eval stack and sets the value
// register to #t if it is a pair, #f otherwise. Cannot fail, so it ignores the
// primitive name execPromoted threads through for diagnostics.
func inlinePairQ(mc *MachineContext, _ string) error {
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
func inlineCar(mc *MachineContext, name string) error {
	o := mc.evals.Pop()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained++
	mc.counters.ForeignCalls++

	p, ok := o.(values.Tuple)
	if !ok || values.IsEmptyList(o) {
		return werr.WrapForeignErrorf(
			werr.ErrNotAPair, "%s: expected pair, got %s", name, o.SchemeString())
	}
	mc.SetValue(p.Car())
	return nil
}

// inlineCdr pops one argument from the eval stack, validates it is a pair,
// and sets the value register to its cdr. Returns nil on success.
func inlineCdr(mc *MachineContext, name string) error {
	o := mc.evals.Pop()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained++
	mc.counters.ForeignCalls++

	p, ok := o.(values.Tuple)
	if !ok || values.IsEmptyList(o) {
		return werr.WrapForeignErrorf(
			werr.ErrNotAPair, "%s: expected pair, got %s", name, o.SchemeString())
	}
	mc.SetValue(p.Cdr())
	return nil
}

// inlineCons pops two arguments (car, cdr) from the eval stack and sets the
// value register to a new pair. No validation needed — cons accepts any values,
// so it ignores the primitive name execPromoted threads through for diagnostics.
func inlineCons(mc *MachineContext, _ string) error {
	cdr, car := mc.evals.Pop2()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += 2
	mc.counters.ForeignCalls++
	mc.SetValue(values.NewCons(car, cdr))
	return nil
}

// execPromoted runs the common promoted-op dispatch pattern: resolve the
// cached binding, verify it is still the expected ForeignClosure, fall back
// to generic call if not, otherwise execute the inline function and handle
// the tail/non-tail epilogue.
func execPromoted(mc *MachineContext, instr Instruction, op *promotedOp, tail bool) (*MachineContext, error) {
	callable := mc.template.cachedBindings[instr.Arg].Value()
	fcls, ok := callable.(*ForeignClosure)
	if !ok || fcls.name != op.name {
		return callPromotedFallback(mc, callable, tail, op.arity)
	}
	err := op.fn(mc, op.name)
	mc.counters.RecordCall(op.name)
	if err != nil {
		// The inline fn returns a RAW error; bridge it here at the (*MC, error)
		// boundary. bridgeForeignError invokes the in-place handler (RaiseInPlace,
		// which reconfigures mc to run INLINE) and returns (mc, nil) so the VM
		// continues into the handler — without advancing pc past the promoted op.
		return bridgeForeignError(mc, err)
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
		return bridgeForeignError(mc, err)
	}
	return result, nil
}

// promotedOpForName returns the non-tail and tail opcodes for a promoted
// primitive, or (OpInvalid, OpInvalid) if the primitive is not promoted.
// Also returns the expected argument count for arity validation.
func promotedOpForName(name string) (nonTail, tail OpCode, arity int) {
	op, ok := promotedByName[name]
	if !ok {
		return OpInvalid, OpInvalid, 0
	}
	return op.nonTail, op.tail, op.arity
}
