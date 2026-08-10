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
// Edit sites (4 places):
//  1. opcode.go           — add OpXxx + OpXxxTail constants inside the promoted
//     range (OpEqQ..OpDivTail); add two opcodeTable entries with
//     operandKind: OperandCachedBinding
//  2. call_promoted.go    — implement inlineXxx(mc *MachineContext, name string) error;
//     mint an IdentityXxx token; add a promotedOp descriptor and list it in
//     promotedOps (or call_promoted_arithmetic.go for numeric ops). The Scheme
//     name, arity, identity, and both opcodes are stated once, in the descriptor
//     — do not restate the name in the inline function's diagnostics; it arrives
//     as the name parameter.
//  3. machine_context.go  — add two case branches in Run() (non-tail + tail),
//     each calling execPromoted(mc, instr, &promotedXxx, tail)
//  4. registry/core/*.go  — declare Identity: machine.IdentityXxx on the
//     primitive's spec. Without it the closure carries no token and the op is
//     never promoted: correct, but a permanent deopt no value assertion sees.
//
// No changes needed in native_template.go or disassemble.go — both use
// opcodeTable[op].operandKind metadata to handle OperandCachedBinding generically.
// The peephole optimizer (peephole.go) also needs no changes — it uses
// promotedOpForIdentity() to discover promoted ops generically.
package machine

import (
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// promotedOp describes one promoted primitive. Every fact about the op is
// stated here exactly once: the identity the cached binding is guarded against,
// the Scheme name (the RecordCall key and the prefix fn's diagnostics use), the
// argument count the fallback path pops, the two opcodes the peephole optimizer
// emits, and the inline implementation. Run()'s switch and promotedOpForIdentity
// both read the descriptor rather than restating any of it.
type promotedOp struct {
	name    string
	arity   int
	nonTail OpCode
	tail    OpCode
	fn      func(*MachineContext, string) error
	// identity is the discriminator, compared by POINTER. name is diagnostics
	// only and is never compared: two closures can spell the same name without
	// being the same primitive, and a Go embedder's own procedure named "cons"
	// must not be dispatched to inlineCons.
	identity *PrimitiveIdentity
	// mutates records whether fn can write through to an object the program can
	// still name. It exists so the immutability ratchet can SELECT the mutators
	// off this table instead of restating a list that drifts from it; a promoted
	// mutator nobody classified would otherwise skip the ratchet silently, which
	// is how the tail-position set-cdr! bypass survived. Pinned by
	// TestPromotedOpsAreClassified.
	mutates bool
}

// The identity token of each promoted primitive, minted once here. The
// registry declares one on the matching PrimitiveSpec (registry/core), which
// stamps it onto every ForeignClosure built from that spec; execPromoted and the
// peephole optimizer then compare it by pointer against the descriptor's.
//
// They are exported only because the specs that declare them live in
// pkg/registry/core, which imports pkg/machine and not the reverse. A Go
// embedder naming one of these in a spec of their own therefore opts INTO
// promoted dispatch deliberately — unlike the accidental name collision this
// replaces, where any closure spelled "cons" was inlined whether it wanted to be
// or not.
var (
	IdentityEqQ       = NewPrimitiveIdentity("eq?")
	IdentityVectorQ   = NewPrimitiveIdentity("vector?")
	IdentityVectorRef = NewPrimitiveIdentity("vector-ref")
	IdentityNullQ     = NewPrimitiveIdentity("null?")
	IdentityPairQ     = NewPrimitiveIdentity("pair?")
	IdentityCar       = NewPrimitiveIdentity("car")
	IdentityCdr       = NewPrimitiveIdentity("cdr")
	IdentityCons      = NewPrimitiveIdentity("cons")
	IdentityAdd       = NewPrimitiveIdentity("+")
	IdentitySub       = NewPrimitiveIdentity("-")
	IdentityMul       = NewPrimitiveIdentity("*")
	IdentityDiv       = NewPrimitiveIdentity("/")
	IdentityNumLt     = NewPrimitiveIdentity("<")
	IdentityNumLe     = NewPrimitiveIdentity("<=")
	IdentityNumGt     = NewPrimitiveIdentity(">")
	IdentityNumGe     = NewPrimitiveIdentity(">=")
	IdentityNumEq     = NewPrimitiveIdentity("=")
	IdentitySetCdr    = NewPrimitiveIdentity("set-cdr!")
)

// One descriptor per promoted primitive. Run()'s case branches reference these
// by address; the shared promotedOps list below drives identity lookup.
var (
	promotedEqQ       = promotedOp{name: "eq?", arity: 2, nonTail: OpEqQ, tail: OpEqQTail, fn: inlineEq, identity: IdentityEqQ}
	promotedVectorQ   = promotedOp{name: "vector?", arity: 1, nonTail: OpVectorQ, tail: OpVectorQTail, fn: inlineVectorQ, identity: IdentityVectorQ}
	promotedVectorRef = promotedOp{name: "vector-ref", arity: 2, nonTail: OpVectorRef, tail: OpVectorRefTail, fn: inlineVectorRef, identity: IdentityVectorRef}
	promotedNullQ     = promotedOp{name: "null?", arity: 1, nonTail: OpNullQ, tail: OpNullQTail, fn: inlineNullQ, identity: IdentityNullQ}
	promotedPairQ     = promotedOp{name: "pair?", arity: 1, nonTail: OpPairQ, tail: OpPairQTail, fn: inlinePairQ, identity: IdentityPairQ}
	promotedCar       = promotedOp{name: "car", arity: 1, nonTail: OpCar, tail: OpCarTail, fn: inlineCar, identity: IdentityCar}
	promotedCdr       = promotedOp{name: "cdr", arity: 1, nonTail: OpCdr, tail: OpCdrTail, fn: inlineCdr, identity: IdentityCdr}
	promotedCons      = promotedOp{name: "cons", arity: 2, nonTail: OpCons, tail: OpConsTail, fn: inlineCons, identity: IdentityCons}
	promotedAdd       = promotedOp{name: "+", arity: 2, nonTail: OpAdd, tail: OpAddTail, fn: inlineAdd, identity: IdentityAdd}
	promotedSub       = promotedOp{name: "-", arity: 2, nonTail: OpSub, tail: OpSubTail, fn: inlineSub, identity: IdentitySub}
	promotedMul       = promotedOp{name: "*", arity: 2, nonTail: OpMul, tail: OpMulTail, fn: inlineMul, identity: IdentityMul}
	promotedDiv       = promotedOp{name: "/", arity: 2, nonTail: OpDiv, tail: OpDivTail, fn: inlineDiv, identity: IdentityDiv}
	promotedNumLt     = promotedOp{name: "<", arity: 2, nonTail: OpNumLt, tail: OpNumLtTail, fn: inlineNumLt, identity: IdentityNumLt}
	promotedNumLe     = promotedOp{name: "<=", arity: 2, nonTail: OpNumLe, tail: OpNumLeTail, fn: inlineNumLe, identity: IdentityNumLe}
	promotedNumGt     = promotedOp{name: ">", arity: 2, nonTail: OpNumGt, tail: OpNumGtTail, fn: inlineNumGt, identity: IdentityNumGt}
	promotedNumGe     = promotedOp{name: ">=", arity: 2, nonTail: OpNumGe, tail: OpNumGeTail, fn: inlineNumGe, identity: IdentityNumGe}
	promotedNumEq     = promotedOp{name: "=", arity: 2, nonTail: OpNumEq, tail: OpNumEqTail, fn: inlineNumEq, identity: IdentityNumEq}
	promotedSetCdr    = promotedOp{name: "set-cdr!", arity: 2, nonTail: OpSetCdr, tail: OpSetCdrTail, fn: inlineSetCdr, identity: IdentitySetCdr, mutates: true}
)

// PromotedMutatorNames returns the Scheme names of the promoted primitives that
// write through to a program-visible object. Exported for the immutability
// ratchet, which must run through the full optimizing pipeline (promoted opcodes
// are the peephole's output) and therefore lives outside this package.
func PromotedMutatorNames() []string {
	q := make([]string, 0, 1)
	for _, op := range promotedOps {
		if op.mutates {
			q = append(q, op.name)
		}
	}
	return q
}

// promotedOps is the registry of promoted primitives. Listing a descriptor here
// is what makes the peephole optimizer aware of it (via promotedOpForIdentity);
// TestPromotedOpsCoverPromotedOpcodes pins that the list covers every opcode in
// the promoted range, so a descriptor left off the list fails the build's tests
// rather than silently disabling promotion for that primitive.
var promotedOps = []*promotedOp{
	&promotedEqQ, &promotedVectorQ, &promotedVectorRef, &promotedNullQ,
	&promotedPairQ, &promotedCar, &promotedCdr, &promotedCons,
	&promotedAdd, &promotedSub, &promotedMul, &promotedDiv,
	&promotedNumLt, &promotedNumLe, &promotedNumGt, &promotedNumGe,
	&promotedNumEq, &promotedSetCdr,
}

// promotedByIdentity indexes promotedOps by primitive identity. Built once at
// init; read only on the compile path (promotedOpForIdentity), never in the VM
// dispatch loop.
var promotedByIdentity = buildPromotedByIdentity()

// buildPromotedByIdentity indexes promotedOps by primitive identity.
func buildPromotedByIdentity() map[*PrimitiveIdentity]*promotedOp {
	q := make(map[*PrimitiveIdentity]*promotedOp, len(promotedOps))
	for _, op := range promotedOps {
		q[op.identity] = op
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

// inlineSetCdr is the promoted set-cdr!. It is the first promoted op that MUTATES
// rather than computing a value, so it carries two obligations the pure ops do not:
// it must reject an immutable literal pair exactly as PrimSetCdr does
// (registry/core/prim_pairs.go), and it must leave the value register holding Void
// rather than the mutated pair.
//
// The *values.Pair type assertion is deliberately narrower than inlineCar's
// values.Tuple: R7RS §6.4 makes set-cdr! a pair-only operation, and the empty list
// implements Tuple, so accepting Tuple here would admit a mutation target with no
// cdr to set.
func inlineSetCdr(mc *MachineContext, name string) error {
	val, o := mc.evals.Pop2()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += 2
	mc.counters.ForeignCalls++

	p, ok := o.(*values.Pair)
	if !ok {
		return werr.WrapForeignErrorf(
			werr.ErrNotAPair, "%s: expected pair, got %s", name, o.SchemeString())
	}
	if mc.ImmutableLiterals().IsImmutable(p) {
		return werr.WrapForeignErrorf(
			werr.ErrImmutablePair, "%s: cannot mutate immutable literal pair", name)
	}
	p.SetCdr(val)
	mc.SetValue(values.Void)
	return nil
}

// execPromoted runs the common promoted-op dispatch pattern: resolve the
// cached binding, verify it is still the expected ForeignClosure, fall back
// to generic call if not, otherwise execute the inline function and handle
// the tail/non-tail epilogue.
func execPromoted(mc *MachineContext, instr Instruction, op *promotedOp, tail bool) (*MachineContext, error) {
	callable := mc.template.cachedBindings[instr.Arg].Value()
	fcls, ok := callable.(*ForeignClosure)
	if !ok || fcls.identity != op.identity {
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

// promotedOpForIdentity returns the non-tail and tail opcodes for a promoted
// primitive, or (OpInvalid, OpInvalid) if the primitive is not promoted.
// Also returns the expected argument count for arity validation.
//
// nil means NONE and is refused before the lookup: a Scheme procedure, a
// primitive whose spec declared no identity, and a non-procedure all arrive as
// nil, and a nil key would otherwise be a live map entry the moment a descriptor
// lost its token.
func promotedOpForIdentity(identity *PrimitiveIdentity) (nonTail, tail OpCode, arity int) {
	if identity == nil {
		return OpInvalid, OpInvalid, 0
	}
	op, ok := promotedByIdentity[identity]
	if !ok {
		return OpInvalid, OpInvalid, 0
	}
	return op.nonTail, op.tail, op.arity
}
