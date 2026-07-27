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
	"fmt"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// inlineEvalsCap is the number of eval stack values stored directly in the
// continuation struct. When the eval stack at save time has ≤ inlineEvalsCap
// elements, values are copied into inlineEvals and the evals pointer is set
// to nil, avoiding a stack pool acquire/release round-trip.
//
// The value 2 covers the common case: most SaveContinuation calls happen
// before compiling sub-expression arguments, so the caller's stack holds
// only prior intermediate results (typically 0-1 items for binary operators,
// 2 for ternary). Profile data from fib(10) confirms save-time depths of
// 0-1 account for >95% of continuations.
const inlineEvalsCap = 2

type MachineContinuation struct {
	vmState
	parent        *MachineContinuation
	promptHandler Closure // Handler invoked on abort to this prompt
	shared        bool    // true if this frame is part of a captured continuation chain
	// Inline eval storage: when the eval stack at save time has ≤ inlineEvalsCap
	// items, values are stored here and evals is set to nil (sentinel).
	//
	// INVARIANT: evals == nil ⟺ values are in inlineEvals[0:inlineEvalsLen].
	//   SaveContinuation sets this when stack depth ≤ inlineEvalsCap.
	//   RestoreAndRelease/Restore detect nil evals and reconstruct from inline slots.
	//   releaseContinuation (pool reset) zeros the struct, clearing inline slots.
	inlineEvalsLen uint8
	inlineEvals    [inlineEvalsCap]values.Value
}

// restoreInlineEvals clears dst and pushes the inline eval values into it.
// Used by Restore and RestoreAndRelease when a continuation has inlined evals
// (evals == nil).
func restoreInlineEvals(dst *Stack, cont *MachineContinuation) {
	dst.Clear()
	for i := uint8(0); i < cont.inlineEvalsLen; i++ {
		dst.Push(cont.inlineEvals[i])
	}
}

// NewMachineContinuation creates a new machine continuation with the given parent, template, environment frame, and initial values.
func NewMachineContinuation(parent *MachineContinuation, tpl *NativeTemplate, env *environment.EnvironmentFrame) *MachineContinuation {
	var depth int
	if parent != nil {
		depth = parent.callDepth + 1
	}
	q := &MachineContinuation{
		vmState: vmState{
			env:       env,
			template:  tpl,
			evals:     NewStack(),
			callDepth: depth,
		},
		parent: parent,
	}
	return q
}

// NewMachineContinuationFromMachineContext creates a new machine continuation from the given machine context and an offset to the program counter.
// The new continuation inherits the environment, template, and evaluation stack from the machine context.
//
// callDepth derivation: the new frame's parent is mc.cont, so its depth is
// derived from mc.cont's cached depth — NOT from mc.callDepth. These values
// differ because SaveContinuation pre-increments mc.callDepth before calling
// this function, but other callers do not:
//
//   - SaveContinuation: mc.callDepth already incremented → mc.callDepth != chain length
//   - CaptureInterruptContinuationAt (timer interrupt): mc.callDepth == 0,
//     mc.cont == nil on a rootless context
//
// Using mc.callDepth - 1 would underflow to -1 in that case (mc.callDepth == 0).
// The parent-pointer formula is correct for all callers and immune to underflow.
func NewMachineContinuationFromMachineContext(mc *MachineContext, off int) *MachineContinuation {
	var depth int
	if mc.cont != nil {
		depth = mc.cont.callDepth + 1
	}
	q := mc.acquireContinuation()
	q.env = mc.env
	q.template = mc.template
	q.copyValueRegisterFrom(&mc.vmState)
	q.evals = mc.evals
	q.pc = mc.pc + off
	q.threadID = mc.threadID
	q.callDepth = depth
	q.envPooled = mc.envPooled
	q.marks = mc.marks
	q.barrierValid = mc.barrierValid
	q.parent = mc.cont
	return q
}

func (p *MachineContinuation) Parent() *MachineContinuation {
	return p.parent
}

func (p *MachineContinuation) EnvironmentFrame() *environment.EnvironmentFrame {
	return p.env
}

func (p *MachineContinuation) Template() *NativeTemplate {
	return p.template
}

func (p *MachineContinuation) PC() int {
	return p.pc
}

func (p *MachineContinuation) SetPC(v int) {
	p.pc = v
}

// PushValues lives on *vmState in vm_state.go; method promotion makes it
// callable on *MachineContinuation.

// CallDepth returns the depth of the continuation stack.
// The depth is cached in each frame at creation time, so this is O(1).
func (p *MachineContinuation) CallDepth() int {
	if p == nil {
		return 0
	}
	return p.callDepth
}

func (p *MachineContinuation) Copy() *MachineContinuation {
	// Allocates from the process-global continuation pool (not a per-thread one):
	// Copy has no *MachineContext receiver, so it cannot reach the caller's
	// threadPools. The resulting frame may later be released via
	// mc.releaseContinuation into a per-thread pool, causing slow asymmetric
	// drift between the global and per-thread pools over a program's life. This
	// is benign — a *MachineContinuation is valid in any freelist of its type,
	// access is same-goroutine sequential (Copy runs on the executing thread;
	// thread-confinement forbids cross-goroutine release), and the drift is an
	// allocation-accounting wrinkle, not a correctness or race issue. A
	// receiver-aware Copy is deliberately not worth threading through this
	// delimited-continuation copy path. See machine/pool.go (threadPools).
	q := acquireContinuation()
	q.env = p.env
	q.template = p.template
	q.cloneValueRegisterFrom(&p.vmState)
	if p.evals != nil {
		q.evals = p.evals.Copy()
	}
	q.pc = p.pc
	if len(p.windingStack) > 0 {
		q.windingStack = p.windingStack.Copy()
	}
	q.promptTag = p.promptTag
	q.threadID = p.threadID
	q.callDepth = p.callDepth
	// envPooled: zero value (false) — Copy() shares the env pointer
	// with the original frame. The copy does not own the env frame
	// and must not release it back to the pool.
	// shared: zero value (false) — copies are always fresh.
	q.marks = cloneMarks(p.marks)
	q.barrierValid = p.barrierValid
	q.parent = p.parent
	q.promptHandler = p.promptHandler
	q.inlineEvalsLen = p.inlineEvalsLen
	q.inlineEvals = p.inlineEvals // array copy (value semantics)
	return q
}

func (p *MachineContinuation) PromptTag() *PromptTag {
	return p.promptTag
}

func (p *MachineContinuation) SetPromptTag(t *PromptTag) {
	p.promptTag = t
}

func (p *MachineContinuation) PromptHandler() Closure {
	return p.promptHandler
}

func (p *MachineContinuation) SetPromptHandler(h Closure) {
	p.promptHandler = h
}

func (p *MachineContinuation) ThreadID() uint64 {
	return p.threadID
}

// NewMachineContinuationWithPrompt creates a continuation frame that acts as
// a continuation prompt. The tag identifies the prompt for abort/capture, and
// the handler is invoked when an abort reaches this prompt.
func NewMachineContinuationWithPrompt(parent *MachineContinuation, tpl *NativeTemplate, env *environment.EnvironmentFrame, tag *PromptTag, handler Closure) *MachineContinuation {
	q := NewMachineContinuation(parent, tpl, env)
	q.promptTag = tag
	q.promptHandler = handler
	return q
}

// DeepCopy creates a deep copy of the entire continuation chain.
// Each frame in the chain is copied, with parent pointers updated to
// point to the copied frames. This is needed for composable continuations
// which must be safely re-invoked multiple times.
func (p *MachineContinuation) DeepCopy() *MachineContinuation {
	if p == nil {
		return nil
	}
	q := p.Copy()
	current := q
	for current.parent != nil {
		parentCopy := current.parent.Copy()
		current.parent = parentCopy
		current = parentCopy
	}
	return q
}

// MarkChainShared marks every frame in the continuation chain as shared.
// Shared frames are not pooled by RestoreAndRelease — their evals are
// copied instead of transferred, preserving them for re-invocation.
//
// Early-exits when a frame is already shared: all ancestors must already
// be shared from a prior capture (sharing propagates toward the root).
func (p *MachineContinuation) MarkChainShared() {
	for frame := p; frame != nil; frame = frame.parent {
		if frame.shared {
			return
		}
		frame.shared = true
	}
}

func (p *MachineContinuation) SchemeString() string {
	return fmt.Sprintf("<machine-continuation %%%d>", p.pc)
}

func (p *MachineContinuation) IsVoid() bool {
	return p == nil
}

// EqualTo compares by identity. A MachineContinuation is an internal
// continuation-chain node, never surfaced to Scheme equal?/eqv? (first-class
// continuations are *CapturedContinuation / *ComposableContinuation), so two
// distinct nodes are never "equal" in any observable sense. The former
// field-by-field comparison was structurally unsound (it mixed pointer-equal
// parent checks with value comparison of the rest) and effectively dead.
func (p *MachineContinuation) EqualTo(o values.Value) bool {
	v, ok := o.(*MachineContinuation)
	if !ok {
		return false
	}
	return p == v
}
