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
	"slices"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// markEntry is a single continuation mark key-value pair.
// Marks are stored as a flat slice; see vmState.marks for rationale.
type markEntry struct {
	key values.Value
	val values.Value
}

// vmState holds the execution state fields shared between MachineContext and
// MachineContinuation. Both types embed this struct so that the shared field
// set is documented in one place and impossible to get out of sync.
//
// CESK machine (Felleisen & Friedman 1987). VM state is a 4-tuple.
//
//	σ = (C, E, S, K), where:
//	  C = (template, pc)  — control: which instruction to execute
//	  E = env             — environment: lexical bindings (frame chain)
//	  S = evals           — store: operand stack for intermediate values
//	  K = cont            — kontinuation: linked list of saved states
//
//	Transitions:
//	  Non-tail call: (C,E,S,K) → (C',E',∅, K·σ)   push frame onto K
//	  Tail call:     (C,E,S,K) → (C',E',∅, K)      K unchanged
//	  Return:        (_,_,_,K·σ) → σ                pop frame from K
//
//	Invariant: K is a heap-allocated linked list, never Go stack frames.
//	  This is what makes call/cc possible — K is capturable data.
//	Constrains: Apply (must produce correct K), SaveContinuation/Restore
//	  (must preserve exactly the fields in σ).
//	Constrained by: de Bruijn addressing (E is indexed by slot,depth),
//	  linked closures (MakeClosure captures E by pointer).
//
// See BIBLIOGRAPHY.md "CESK Abstract Machine".
//
// # Value register encoding
//
// The value register uses a split representation to avoid heap allocation on
// every bytecode instruction. Nearly all VM operations produce a single value;
// R7RS multiple return values (values / call-with-values) are rare.
//
//   - singleValue: holds the result when exactly one value is produced.
//     Setting it requires no allocation (just an interface assignment).
//   - multiValues: non-nil only when multiple values are in play
//     (SetValues with len > 1, R7RS values primitive).
//
// Invariant: at most one of the two fields is "active" at any time.
// When multiValues != nil it is authoritative; otherwise singleValue is.
// SetValue nils multiValues; SetValues with len > 1 nils singleValue.
//
// This eliminated ~20% of all allocations in call-heavy benchmarks by
// removing the []values.Value{v} slice that every operation previously created.
// See BIBLIOGRAPHY.md "Split Value Register".
//
// IMPORTANT: The fields are NOT uniformly copied by save/restore operations.
// The table below summarizes how each method (SaveContinuation, Restore,
// PopContinuation) treats each field.
//
//	┌──────────────┬────────────────┬─────────────┬─────────────────────┬──────────────────┐
//	│ Field        │ SaveCont saves │ Restore     │ RestoreAndRelease   │ PopContinuation  │
//	├──────────────┼────────────────┼─────────────┼─────────────────────┼──────────────────┤
//	│ env          │ ✓              │ ✓           │ ✓                   │ ✓                │
//	│ template     │ ✓              │ ✓           │ ✓                   │ ✓                │
//	│ singleValue  │ ✓              │ ✗           │ ✗                   │ ✓                │
//	│ multiValues  │ ✓              │ ✗           │ ✗                   │ ✓                │
//	│ evals        │ ✓              │ ✓ (Copy)    │ ✓ (transfer+pool)   │ ✓ (no copy)      │
//	│ pc           │ ✓ (+offset)    │ ✓           │ ✓                   │ ✓                │
//	│ threadID     │ ✓              │ ✗           │ ✗                   │ ✗                │
//	│ windingStack │ ✗              │ ✗           │ ✗                   │ ✗                │
//	│ promptTag    │ ✗              │ ✗           │ ✗                   │ ✗                │
//	│ callDepth    │ ✓              │ ✓ (saved)   │ ✓ (saved)           │ ✗                │
//	│ envPooled    │ ✓              │ ✗ (=false)  │ ✓ or false(shared)  │ ✓                │
//	│ marks        │ ✓              │ ✓           │ ✓                   │ ✓                │
//	└──────────────┴────────────────┴─────────────┴─────────────────────┴──────────────────┘
type vmState struct {
	env      *environment.EnvironmentFrame
	template *NativeTemplate
	// singleValue and multiValues form a split value register.
	//
	// Only one field is live at a time — the other is always nil.
	// This is a performance optimisation: storing a single value in singleValue
	// avoids the []Value{v} allocation that SetValues would otherwise cause on
	// every load, store, and arithmetic instruction.
	//
	// INVARIANT: the two fields are mutually exclusive.
	//   SetValue  sets singleValue and nils multiValues.
	//   SetValues sets multiValues and nils singleValue (except for the
	//   len==1 fast path, which delegates to SetValue).
	//
	// multiValues is only populated by R7RS (values ...) and is consumed by
	// call-with-values. All other instructions operate on singleValue.
	singleValue values.Value   // value register: single value (fast path)
	multiValues MultipleValues // value register: multiple values (R7RS values/call-with-values only)
	evals       *Stack         // evaluation stack, holds intermediate values during execution
	// pc is the program counter: index into template.code.
	// Run() does NOT reset pc — it starts from the current value.
	//
	// Write sites:
	//   - NewMachineContext: copies from continuation (typically 0)
	//   - Apply: sets to 0 for fresh closure invocation
	//   - Restore/RestoreAndRelease/PopContinuation: restores saved pc
	//   - SaveContinuation: saves pc + offset into continuation
	//   - Opcodes: mc.pc++ or mc.pc += offset (branches)
	//
	// This design enables raise-continuable resumption: Restore sets pc
	// to the instruction after the handler call, and Run() continues
	// from there rather than restarting at 0.
	pc           int
	windingStack WindingStack // R7RS dynamic-wind extent tracking
	promptTag    *PromptTag   // prompt tag for continuation prompts
	// threadID is the SRFI-18 thread identity (0 = primordial thread).
	//
	// Thread identity is split across two representations:
	//
	//   vmState.threadID (uint64)       — propagates into continuations
	//   MachineContext.thread (*Thread)  — stays on the live context only
	//
	// Why two fields:
	//
	//   threadID lives in vmState, which is embedded by both MachineContext and
	//   MachineContinuation. Every SaveContinuation stamps the current threadID
	//   into the saved frame. Continuations need it for cross-thread rejection:
	//
	//     - applyComposableContinuation (machine_context.go) compares
	//       mc.threadID against cc.threadID.
	//     - call/cc escape closures (prim_control.go, prim_exit.go) compare
	//       the invoking context's ThreadID() against the capturing thread's.
	//     - ComposableContinuation stores its own threadID copy, also set from
	//       mc.ThreadID() at capture time (prim_prompt.go, prim_control.go).
	//
	//   thread (*values.Thread) is NOT in vmState because continuations never
	//   need the Scheme object — only the numeric ID for comparison. Storing a
	//   pointer in vmState would add heap pressure to every continuation frame
	//   for no benefit. The Scheme object is needed only by:
	//
	//     - current-thread (prim_threads.go:83) — returns mc.Thread() to Scheme.
	//     - thread-specific mutation (prim_threads.go:405) — needs the owner.
	//     - eval sub-contexts (prim_eval.go) — propagates thread via SetThread().
	//
	// Invariant: when MachineContext.thread is non-nil,
	//   thread.ID() == threadID.
	// Enforced by SetThread(), which is the single write path for both fields.
	//
	// Propagation:
	//   NewSubContext      — copies both threadID and thread from parent.
	//   NewThreadSubContext — sets both via SetThread(newThread).
	//   SaveContinuation   — stamps threadID into the continuation frame.
	//   Restore            — does NOT restore threadID (invoking thread keeps its own).
	threadID uint64
	// callDepth caches the continuation chain length to avoid O(d) traversals.
	//
	// On MachineContext: number of frames in the cont chain (mc.cont → ... → nil).
	//   Maintained by SaveContinuation (++), PopContinuation (--), Restore (read
	//   from continuation).
	//
	// On MachineContinuation: number of ancestor frames (parent → ... → nil).
	//   Set once at creation time, never mutated. A root frame (parent == nil)
	//   has callDepth 0; each level adds 1.
	//
	// All depth computation should use the parent pointer (which is nil-safe)
	// rather than arithmetic on callDepth. See NewMachineContinuationFromMachineContext.
	callDepth int
	// envPooled is true when env was acquired from the envFramePool (Apply
	// copy path). When the env is about to be overwritten (RestoreAndRelease,
	// Restore), this flag tells us it's safe to return it to the pool.
	// Set false for sub-context envs or envs restored from shared
	// continuations.
	//
	// INVARIANT: every site that writes mc.env MUST also set mc.envPooled.
	// Failing to do so can cause releaseEnvFrame on a non-pooled frame
	// (use-after-release if a closure still references it).
	//
	// Write sites and release semantics:
	//
	//  ┌──────────────────────────┬──────────┬───────────────────────────────────┐
	//  │ Site                     │ envPooled│ Release of old env                │
	//  ├──────────────────────────┼──────────┼───────────────────────────────────┤
	//  │ Apply (copy path)        │ true     │ none (old env is in continuation) │
	//  │ Apply (nil-parent path)  │ false    │ none (old env is in continuation) │
	//  │ RestoreAndRelease        │ from cont│ yes, if oldPooled && old != new   │
	//  │ Restore (shared/callcc)  │ false    │ no (may be in shared chain → GC)  │
	//  │ PopContinuation          │ from cont│ no (caller manages old frame)     │
	//  │ OpPopEnv                 │ false    │ no (parent was never pooled)      │
	//  │ OpMakeClosure            │ false    │ no (closure takes ownership)      │
	//  │ BindPatternVars          │ false    │ no (childEnv is heap-allocated)   │
	//  │ NewSubContext            │ false(0) │ n/a (fresh context)               │
	//  │ acquireMacroContext      │ false(0) │ n/a (fresh context)               │
	//  └──────────────────────────┴──────────┴───────────────────────────────────┘
	envPooled bool
	// marks holds per-frame continuation marks (Racket-style key-value annotations).
	// nil when no marks are set on this frame (zero-cost common case).
	// Lazily allocated by SetMark on first use.
	//
	// Stored as a flat slice rather than a map. Key lookup uses values.EqIdentity
	// (eq? semantics: symbol identity by name, all other types by pointer).
	// Mark counts per frame are typically 0–3, so O(n) scan is O(1) in practice
	// and avoids the Go-map comparability requirement that caused panics for
	// non-comparable value types (MultipleValues) and wrong results for symbols
	// after interning removal.
	//
	// Propagation: SaveContinuation copies to continuation then nils mc.marks
	// (callee starts clean). Restore/PopContinuation restores from continuation.
	// cloneMarks does a shallow copy for call/cc re-invocation safety.
	marks []markEntry
}

// Value-register accessors.
//
// All reads and writes of singleValue / multiValues must go through these
// methods, so the documented mutual-exclusion invariant (at most one field
// is active at a time) is enforceable by inspection of this file alone.
// Enforced at lint time by noDirectValueRegisterAccess in
// ruleguard/rules.go. See memory/2026-05-11-machine-sr-finding3-impl.md.

// SetValues sets the value register. Three paths:
//   - len == 0: canonical empty state, both fields nil. Distinguishes
//     the (nil, nil) empty register from (nil, []) which a naive
//     'multiValues = vs' would produce when vs is an empty-but-non-nil
//     spread (e.g. SetValues(emptySlice...)).
//   - len == 1: zero-allocation fast path via singleValue.
//   - len  > 1: fall back to the multiValues slice.
func (p *vmState) SetValues(vs ...values.Value) {
	if len(vs) == 0 {
		p.singleValue = nil
		p.multiValues = nil
		return
	}
	if len(vs) == 1 {
		p.singleValue = vs[0]
		p.multiValues = nil
		return
	}
	p.multiValues = vs
	p.singleValue = nil
}

// SetValue stores a single value in the value register without allocating.
// This is the hot path: every LoadLocal, LoadGlobal, LoadLiteral, Pull, Pop,
// MakeClosure, etc. goes through here.
func (p *vmState) SetValue(v values.Value) {
	p.singleValue = v
	p.multiValues = nil
}

// GetValue returns the first (or only) value from the value register.
// Returns Void if the register is empty.
func (p *vmState) GetValue() values.Value {
	if p.multiValues != nil {
		if len(p.multiValues) == 0 {
			return values.Void
		}
		return p.multiValues[0]
	}
	if p.singleValue == nil {
		return values.Void
	}
	return p.singleValue
}

// GetValues returns all values from the value register as a MultipleValues
// slice. For the single-value case this allocates a one-element slice; callers
// on the hot path should use GetValue instead.
func (p *vmState) GetValues() MultipleValues {
	if p.multiValues != nil {
		return p.multiValues
	}
	if p.singleValue == nil {
		return nil
	}
	return MultipleValues{p.singleValue}
}

// PushValues appends values to the value register. If the register currently
// holds a single value, it is promoted to the multi-value representation
// before appending. This promote-then-append pattern avoids losing the
// existing single value when transitioning to the multi-value path.
//
// Order matters: we nil singleValue *before* installing the promoted
// MultipleValues so the mutual-exclusion invariant is never violated, even
// in the intra-method transient window.
func (p *vmState) PushValues(v ...values.Value) {
	if p.multiValues == nil && p.singleValue != nil {
		promoted := MultipleValues{p.singleValue}
		p.singleValue = nil
		p.multiValues = promoted
	}
	p.multiValues = append(p.multiValues, v...)
}

// The three helpers below (pushValueRegisterTo, copyValueRegisterFrom,
// cloneValueRegisterFrom) are unexported deliberately. They serve
// machine-internal save/restore/dispatch flows and have no use outside
// this package. Exporting them would extend method promotion to
// *MachineContext / *MachineContinuation and grow the public API surface
// for no benefit.

// pushValueRegisterTo pushes the live half of the value register onto s.
// Used by OpPush. Preserves the single-value fast path: no MultipleValues
// wrap, no allocation when only singleValue is live.
func (p *vmState) pushValueRegisterTo(s *Stack) {
	if p.multiValues != nil {
		s.PushAll(p.multiValues)
		return
	}
	if p.singleValue != nil {
		s.Push(p.singleValue)
	}
}

// copyValueRegisterFrom copies both halves of the value register from src.
// Shallow: multiValues is shared (slice header copy). Used by
// SaveContinuation, PopContinuation, and NewMachineContext initialization
// — sites where the source and destination represent the same logical
// register state across a save/restore boundary.
func (p *vmState) copyValueRegisterFrom(src *vmState) {
	p.singleValue = src.singleValue
	p.multiValues = src.multiValues
}

// cloneValueRegisterFrom copies the value register from src with the
// multiValues slice deep-copied via slices.Clone. Used by
// MachineContinuation.Copy() to support re-invocable continuations (call/cc):
// each invocation must see an independent slice so PushValues append does not
// corrupt the original.
func (p *vmState) cloneValueRegisterFrom(src *vmState) {
	p.singleValue = src.singleValue
	p.multiValues = slices.Clone(src.multiValues)
}
