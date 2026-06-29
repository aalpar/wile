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
	"reflect"
	"sort"
	"testing"

	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestVmState(t *testing.T) {
	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "zero value has nil fields",
			checkFn: func(t *testing.T) {
				var s vmState
				qt.Assert(t, s.env, qt.IsNil)
				qt.Assert(t, s.template, qt.IsNil)
				qt.Assert(t, s.singleValue, qt.IsNil)
				qt.Assert(t, s.multiValues, qt.IsNil)
				qt.Assert(t, s.evals, qt.IsNil)
				qt.Assert(t, s.pc, qt.Equals, 0)
				qt.Assert(t, s.windingStack, qt.IsNil)
				qt.Assert(t, s.promptTag, qt.IsNil)
				qt.Assert(t, s.threadID, qt.Equals, uint64(0))
				qt.Assert(t, s.callDepth, qt.Equals, 0)
				qt.Assert(t, s.envPooled, qt.IsFalse)
				qt.Assert(t, s.marks, qt.IsNil)
			},
		},
		{
			name: "fields can be set directly",
			checkFn: func(t *testing.T) {
				s := vmState{
					pc:        42,
					threadID:  7,
					callDepth: 3,
					envPooled: true,
				}
				qt.Assert(t, s.pc, qt.Equals, 42)
				qt.Assert(t, s.threadID, qt.Equals, uint64(7))
				qt.Assert(t, s.callDepth, qt.Equals, 3)
				qt.Assert(t, s.envPooled, qt.IsTrue)
			},
		},
		{
			name: "split value register: singleValue and multiValues are independent",
			checkFn: func(t *testing.T) {
				s := vmState{
					singleValue: values.NewInteger(1),
				}
				qt.Assert(t, s.singleValue, qt.IsNotNil)
				qt.Assert(t, s.multiValues, qt.IsNil)

				s.multiValues = MultipleValues{values.NewInteger(2), values.NewInteger(3)}
				s.singleValue = nil
				qt.Assert(t, s.singleValue, qt.IsNil)
				qt.Assert(t, s.multiValues, qt.HasLen, 2)
			},
		},
		{
			name: "marks initially nil",
			checkFn: func(t *testing.T) {
				var s vmState
				qt.Assert(t, s.marks, qt.IsNil)
			},
		},
		{
			// pushValueRegisterTo is a silent no-op when the register is
			// empty (both fields nil). This corresponds to R7RS (values) —
			// the zero-value return — and is the OpPush behavior when the
			// previous instruction did not produce a value.
			name: "pushValueRegisterTo on empty register pushes nothing",
			checkFn: func(t *testing.T) {
				var s vmState
				stack := NewStack()
				s.pushValueRegisterTo(stack)
				qt.Assert(t, stack.Len(), qt.Equals, 0)
			},
		},
		{
			// Sanity-check the single-value fast path: no MultipleValues
			// wrap, no allocation; one element lands on the stack.
			name: "pushValueRegisterTo with single value pushes one",
			checkFn: func(t *testing.T) {
				var s vmState
				s.SetValue(values.NewInteger(42))
				stack := NewStack()
				s.pushValueRegisterTo(stack)
				qt.Assert(t, stack.Len(), qt.Equals, 1)
			},
		},
		{
			name: "pushValueRegisterTo with multi-values pushes all",
			checkFn: func(t *testing.T) {
				var s vmState
				s.SetValues(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
				stack := NewStack()
				s.pushValueRegisterTo(stack)
				qt.Assert(t, stack.Len(), qt.Equals, 3)
			},
		},
		{
			name: "field coverage: every vmState field documented in every operation",
			checkFn: func(t *testing.T) {
				testVmStateFieldCoverage(t)
			},
		},
		{
			name: "windingStack can be set",
			checkFn: func(t *testing.T) {
				var s vmState
				f := NewDynamicWindFrame(nil, nil)
				s.windingStack = WindingStack{f}
				qt.Assert(t, s.windingStack.Depth(), qt.Equals, 1)
			},
		},
		{
			name: "promptTag can be set",
			checkFn: func(t *testing.T) {
				var s vmState
				tag := NewPromptTag("test")
				s.promptTag = tag
				qt.Assert(t, s.promptTag, qt.Equals, tag)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}

// vmStateFieldCoverage documents how each transfer operation handles every
// vmState field. When a field is added to vmState, this table must be updated
// for every operation — the test below enforces this via reflection.
//
// Values describe the handling semantics:
//
//	"copy"          — field value copied directly
//	"copy+offset"   — copied with arithmetic adjustment
//	"clone"         — deep/shallow copy (slices, marks)
//	"transfer"      — ownership moved, source zeroed
//	"conditional"   — behavior depends on runtime state (e.g., shared flag)
//	"derived"       — computed from other state, not copied directly
//	"skip"          — intentionally not transferred
//	"zero"          — receives Go zero value (fresh struct)
//	"force false"   — explicitly set to false regardless of source
var vmStateFieldCoverage = map[string]map[string]string{
	// NewMachineContinuationFromMachineContext: mc → continuation (SaveContinuation path)
	"NewMachineContinuationFromMachineContext": {
		"barrierValid": "copy (stamp current barrier onto the saved frame)",
		"env":          "copy",
		"template":     "copy",
		"singleValue":  "copy",
		"multiValues":  "copy",
		"evals":        "copy (alias; caller decides inline vs transfer)",
		"pc":           "copy+offset",
		"windingStack": "skip (not saved per-frame)",
		"promptTag":    "skip (not saved per-frame)",
		"threadID":     "copy",
		"callDepth":    "derived (from mc.cont parent chain)",
		"envPooled":    "copy",
		"marks":        "copy (SaveContinuation nils mc.marks after)",
	},
	// Restore: continuation → mc (call/cc re-entry, composable continuation)
	"Restore": {
		"barrierValid": "copy (restore the barrier the frame was created under)",
		"env":          "copy",
		"template":     "copy",
		"singleValue":  "skip (caller's value preserved)",
		"multiValues":  "skip (caller's value preserved)",
		"evals":        "clone (must copy for re-invocation safety)",
		"pc":           "copy",
		"windingStack": "skip (winding restored separately)",
		"promptTag":    "skip (prompt context unchanged)",
		"threadID":     "skip (invoking thread keeps its own)",
		"callDepth":    "copy (from continuation's cached depth)",
		"envPooled":    "force false (shared continuation, env must not be pooled)",
		"marks":        "clone",
	},
	// RestoreAndRelease: continuation → mc (normal function return fast path)
	"RestoreAndRelease": {
		"barrierValid": "copy (before the shared/unshared branch)",
		"env":          "copy",
		"template":     "copy",
		"singleValue":  "skip (caller's value preserved)",
		"multiValues":  "skip (caller's value preserved)",
		"evals":        "conditional (transfer if unshared, clone if shared)",
		"pc":           "copy",
		"windingStack": "skip (winding restored separately)",
		"promptTag":    "skip (prompt context unchanged)",
		"threadID":     "skip (invoking thread keeps its own)",
		"callDepth":    "copy",
		"envPooled":    "conditional (from cont if unshared, force false if shared)",
		"marks":        "conditional (transfer if unshared, clone if shared)",
	},
	// PopContinuation: continuation → mc (used by Run loop after RestoreContinuation opcode)
	"PopContinuation": {
		"barrierValid": "copy",
		"env":          "copy",
		"template":     "copy",
		"singleValue":  "copy",
		"multiValues":  "copy",
		"evals":        "transfer (no copy; continuation consumed once)",
		"pc":           "copy",
		"windingStack": "skip (not per-frame)",
		"promptTag":    "skip (not per-frame)",
		"threadID":     "skip (thread identity unchanged)",
		"callDepth":    "derived (decremented before pop)",
		"envPooled":    "copy",
		"marks":        "transfer",
	},
	// Copy: continuation → continuation (for DeepCopy, SliceContinuationAt)
	"Copy": {
		"barrierValid": "copy",
		"env":          "copy",
		"template":     "copy",
		"singleValue":  "copy",
		"multiValues":  "clone (slices.Clone)",
		"evals":        "conditional (evals.Copy if non-nil, else nil)",
		"pc":           "copy",
		"windingStack": "conditional (windingStack.Copy if non-empty)",
		"promptTag":    "copy",
		"threadID":     "copy",
		"callDepth":    "copy",
		"envPooled":    "zero (false; copy shares env, must not release)",
		"marks":        "clone",
	},
	// NewMachineContext: continuation → mc (top-level context creation)
	"NewMachineContext": {
		"barrierValid": "zero (no barrier at top level)",
		"env":          "copy",
		"template":     "copy",
		"singleValue":  "copy",
		"multiValues":  "copy",
		"evals":        "copy (or reconstruct from inline slots)",
		"pc":           "copy",
		"windingStack": "zero (fresh context)",
		"promptTag":    "zero (fresh context)",
		"threadID":     "zero (primordial thread)",
		"callDepth":    "zero (fresh call stack)",
		"envPooled":    "zero (false; not from pool)",
		"marks":        "zero (fresh context)",
	},
	// NewSubContext: mc → mc (sub-context for foreign calls)
	"NewSubContext": {
		"barrierValid": "copy (inherit parent's barrier context)",
		"env":          "derived (parent.env.TopLevel())",
		"template":     "zero (no template; caller sets via Apply)",
		"singleValue":  "zero (fresh value register)",
		"multiValues":  "zero (fresh value register)",
		"evals":        "zero (fresh stack from pool)",
		"pc":           "zero (fresh context)",
		"windingStack": "copy (inherit parent's winding stack)",
		"promptTag":    "zero (no prompt on sub-context)",
		"threadID":     "copy",
		"callDepth":    "zero (fresh call stack)",
		"envPooled":    "zero (false; top-level env)",
		"marks":        "zero (fresh context)",
	},
}

// testVmStateFieldCoverage uses reflection to enumerate vmState fields and
// verifies every field appears in every operation's coverage entry. This
// prevents silent state corruption when fields are added to vmState without
// updating all transfer operations.
func testVmStateFieldCoverage(t *testing.T) {
	typ := reflect.TypeFor[vmState]()
	var fieldNames []string
	for i := range typ.NumField() {
		fieldNames = append(fieldNames, typ.Field(i).Name)
	}
	sort.Strings(fieldNames)

	// Check 1: every field documented in every operation.
	for opName, handling := range vmStateFieldCoverage {
		for _, field := range fieldNames {
			_, ok := handling[field]
			if !ok {
				t.Errorf("operation %q does not document handling of vmState field %q — "+
					"add an entry to vmStateFieldCoverage", opName, field)
			}
		}
	}

	// Check 2: no stale entries (fields removed from vmState but still in table).
	fieldSet := make(map[string]bool, len(fieldNames))
	for _, f := range fieldNames {
		fieldSet[f] = true
	}
	for opName, handling := range vmStateFieldCoverage {
		for field := range handling {
			if !fieldSet[field] {
				t.Errorf("operation %q documents field %q which no longer exists in vmState — "+
					"remove the stale entry from vmStateFieldCoverage", opName, field)
			}
		}
	}
}
