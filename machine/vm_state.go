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
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
)

// vmState holds the execution state fields shared between MachineContext and
// MachineContinuation. Both types embed this struct so that the shared field
// set is documented in one place and impossible to get out of sync.
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
	singleValue  values.Value   // value register: single value (fast path)
	multiValues  MultipleValues // value register: multiple values (R7RS values/call-with-values only)
	evals        *Stack         // evaluation stack, holds intermediate values during execution
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
	// Set false for noCopyApply (closure's own env), sub-context envs, or
	// envs restored from shared continuations.
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
	//  │ Apply (noCopy path)      │ false    │ none (old env is in continuation) │
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
}
