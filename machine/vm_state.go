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
//     (OperationPopAll, SetValues with len > 1).
//
// Invariant: at most one of the two fields is "active" at any time.
// When multiValues != nil it is authoritative; otherwise singleValue is.
// SetValue nils multiValues; SetValues with len > 1 nils singleValue.
//
// This eliminated ~20% of all allocations in call-heavy benchmarks by
// removing the []values.Value{v} slice that every operation previously created.
//
// IMPORTANT: The fields are NOT uniformly copied by save/restore operations.
// The table below summarizes how each method (SaveContinuation, Restore,
// PopContinuation) treats each field.
//
//	┌──────────────┬────────────────┬─────────────┬──────────────────┐
//	│ Field        │ SaveCont saves │ Restore     │ PopContinuation  │
//	├──────────────┼────────────────┼─────────────┼──────────────────┤
//	│ env          │ ✓              │ ✓           │ ✓                │
//	│ template     │ ✓              │ ✓           │ ✓                │
//	│ singleValue  │ ✓              │ ✗           │ ✓                │
//	│ multiValues  │ ✓              │ ✗           │ ✓                │
//	│ evals        │ ✓              │ ✓ (Copy)    │ ✓ (no copy)      │
//	│ pc           │ ✓ (+offset)    │ ✓           │ ✓                │
//	│ threadID     │ ✓              │ ✗           │ ✗                │
//	│ windingStack │ ✗              │ ✗           │ ✗                │
//	│ promptTag    │ ✗              │ ✗           │ ✗                │
//	│ callDepth    │ ✓              │ recomputed  │ ✗                │
//	└──────────────┴────────────────┴─────────────┴──────────────────┘
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
	callDepth    uint64       // current continuation depth (incremented on save, decremented on pop)
}
