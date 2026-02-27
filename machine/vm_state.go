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
	env          *environment.EnvironmentFrame
	template     *NativeTemplate
	singleValue  values.Value   // value register: single value (fast path, no allocation)
	multiValues  MultipleValues // value register: multiple values (only for R7RS values/call-with-values)
	evals        *Stack         // evaluation stack, holds intermediate values during execution
	pc           int
	windingStack WindingStack // R7RS dynamic-wind extent tracking
	promptTag    *PromptTag   // prompt tag for continuation prompts
	threadID     uint64       // SRFI-18 thread identity: 0 = primordial thread
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
