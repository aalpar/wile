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
)

// --- Apply ---

// OperationApply is the bytecode operation that dispatches procedure calls.
// The compiler emits it after pushing arguments onto the eval stack and placing
// the callee in the value register. Apply pops all arguments and delegates to
// MachineContext.ApplyCallable, which handles the six callable types
// (MachineClosure, ForeignClosure, CaseLambdaClosure, Parameter,
// ComposableContinuation, CapturedContinuation).
type OperationApply struct {
	OperationBase
}

// NewOperationApply returns a new apply operation.
func NewOperationApply() *OperationApply {
	return &OperationApply{
		OperationBase: NewOperationBase("operation-apply"),
	}
}

// EqualTo returns true if o is also an OperationApply (identity by type).
func (p *OperationApply) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationApply)
	return SameType(p, v, ok)
}

// --- SelfTailCall ---

// OperationSelfTailCall is the in-place self-recursive tail call: it pops PopCount
// intermediate env frames, drains the ArgCount already-evaluated argument values
// off the eval stack, writes them into the parameter frame's slots 0..ArgCount-1
// (parallel assignment — the args are on the stack, so old slot values stay intact
// during evaluation), and resets pc=0. No frame acquire, no SaveContinuation, no
// continuation growth.
//
// PopCount is the number of `let` frames lexically between the parameter frame and
// the call, and it is why this op is ONE instruction rather than a pop sequence
// followed by a rebind. Between the pops and the rebind, mc.env points at a frame
// whose slots are about to be overwritten while its arguments sit on the eval
// stack; splitting that across instructions makes the intermediate state
// representable to the peephole pass and to anything that can interpose. Popping
// is also the one thing that must not use OpPopEnv: that op clears envPooled as a
// statement about the frame it pops TO, which is exactly the fact this op needs to
// leave alone.
//
// PopCount == 0 encodes byte-identically to the pre-Phase-C operand, so every
// depth-0 site's instruction stream is unchanged.
//
// Emitted only behind validate.BodyIsSelfTailReusable at a self-tail call site;
// that proof (no capture operator anywhere in the body, no escaping closure,
// non-variadic, no set! of the self name) is what makes reusing the live frame
// sound, and it covers let bodies for the same reason it covers the top level —
// it walks the whole body. So the popped frames are unreachable except through
// mc.env by the same argument that licenses the rebind (escape-gated plan Phase 4,
// widened to depth>0 by frame-reclaim Phase C).
type OperationSelfTailCall struct {
	OperationBase
	ArgCount int
	PopCount int
}

// NewOperationSelfTailCall returns a self-tail-call op that pops popCount
// intermediate frames and then rebinds argCount parameter slots.
func NewOperationSelfTailCall(argCount, popCount int) *OperationSelfTailCall {
	return &OperationSelfTailCall{
		OperationBase: NewOperationBaseWithGoName("operation:self-tail-call", "SelfTailCall"),
		ArgCount:      argCount,
		PopCount:      popCount,
	}
}

// EqualTo returns true if o is also an OperationSelfTailCall with the same arity
// and pop count. Both are compared: two sites that agree on arity but not on depth
// are different instructions, and merging them would rebind the wrong frame.
func (p *OperationSelfTailCall) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationSelfTailCall)
	return FieldMatches(p, v, ok, func(op *OperationSelfTailCall) [2]int {
		return [2]int{op.ArgCount, op.PopCount}
	})
}

// --- ReleaseEnvFrame ---

// OperationReleaseEnvFrame releases the current pool-owned env frame back to the
// FreeList immediately before a tail call in a frame-releasable body (no capture,
// no escaping closure, only capture-safe callees). The frame is dead at this point
// — the tail call's args are already on the eval stack — so the next acquire
// reuses it, giving O(1) steady-state frame allocation for fib-shaped recursion.
// A no-op when the frame is not pool-owned (parentless thunk, continuation-shared).
type OperationReleaseEnvFrame struct {
	OperationBase
}

// NewOperationReleaseEnvFrame returns a release-env-frame op.
func NewOperationReleaseEnvFrame() *OperationReleaseEnvFrame {
	return &OperationReleaseEnvFrame{
		OperationBase: NewOperationBaseWithGoName("operation:release-env-frame", "ReleaseEnvFrame"),
	}
}

// EqualTo returns true if o is also an OperationReleaseEnvFrame (identity by type).
func (p *OperationReleaseEnvFrame) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationReleaseEnvFrame)
	return SameType(p, v, ok)
}

// --- UnpackListToStack ---

// OperationUnpackListToStack reads a proper list from the value register
// and pushes each element to the eval stack in order. Used by compiled
// (apply proc arg1 ... args) to flatten the final arg list onto the stack
// before Pull + OpApply.
//
// Errors if the value is not a proper list (improper list or non-list).
type OperationUnpackListToStack struct {
	OperationBase
}

// NewOperationUnpackListToStack returns a new unpack-list-to-stack operation.
func NewOperationUnpackListToStack() *OperationUnpackListToStack {
	return &OperationUnpackListToStack{
		OperationBase: NewOperationBase("operation-unpack-list-to-stack"),
	}
}

// EqualTo returns true if o is also an OperationUnpackListToStack (identity by type).
func (p *OperationUnpackListToStack) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationUnpackListToStack)
	return SameType(p, v, ok)
}
