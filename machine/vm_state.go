package machine

import (
	"github.com/aalpar/wile/environment"
)

// vmState holds the execution state fields shared between MachineContext and
// MachineContinuation. Both types embed this struct so that the shared field
// set is documented in one place and impossible to get out of sync.
//
// IMPORTANT: The fields are NOT uniformly copied by save/restore operations.
// See the per-method field tables in the doc comments for Restore,
// PopContinuation, and SaveContinuation in machine_context.go.
//
//	┌──────────────┬────────────────┬─────────────┬──────────────────┐
//	│ Field        │ SaveCont saves │ Restore     │ PopContinuation  │
//	├──────────────┼────────────────┼─────────────┼──────────────────┤
//	│ env          │ ✓              │ ✓           │ ✓                │
//	│ template     │ ✓              │ ✓           │ ✓                │
//	│ value        │ ✓              │ ✗           │ ✓                │
//	│ evals        │ ✓              │ ✓ (Copy)    │ ✓ (no copy)      │
//	│ pc           │ ✓ (+offset)    │ ✓           │ ✓                │
//	│ threadID     │ ✓              │ ✗           │ ✗                │
//	│ windingStack │ ✗              │ ✗           │ ✗                │
//	│ promptTag    │ ✗              │ ✗           │ ✗                │
//	└──────────────┴────────────────┴─────────────┴──────────────────┘
type vmState struct {
	env          *environment.EnvironmentFrame
	template     *NativeTemplate
	value        MultipleValues
	evals        *Stack // evaluation stack, holds intermediate values during execution
	pc           int
	windingStack WindingStack // R7RS dynamic-wind extent tracking
	promptTag    *PromptTag   // prompt tag for continuation prompts
	threadID     uint64       // SRFI-18 thread identity: 0 = primordial thread
}
