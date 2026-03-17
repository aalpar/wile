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

// InsertBoxes rewrites bytecodes in the defining scope to insert box
// operations for variables that are both captured AND mutated.
//
// This is Pass 2 of the flat closure pipeline. It modifies bytecodes
// in the DEFINING template (not the capturing scope).
//
// Boxing criterion: a variable needs boxing when it is both captured
// (appears in a nested template's FreeVarInfo.Captures with SourceDepth == 1)
// and mutated (appears in any scope's FreeVarInfo.Mutated at the
// corresponding depth, or is written by OpStoreLocal at depth == 0 in the
// defining template's own bytecodes).
//
// Execution order: bottom-up. Inner templates are processed before
// outer templates (same as AnalyzeFreeVars).
func InsertBoxes(tpl *NativeTemplate) {
	// Step 1: Recurse into sub-templates first (bottom-up).
	// Skip sub-templates already processed by a nested compileClosureBody call.
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*NativeTemplate)
		if !ok {
			continue
		}
		if sub.flatClosuresDone {
			continue
		}
		InsertBoxes(sub)
	}

	// Step 2: Collect the set of local slots that need boxing.
	boxedSlots := collectBoxedSlots(tpl)
	if len(boxedSlots) == 0 {
		return
	}

	// Step 3: Rewrite reads and writes of boxed variables.
	// This must happen BEFORE the preamble insertion because the preamble
	// inserts at position 0 via [0,0), and a read/write Replace at [0,1)
	// with the same start could cause an overlap panic in EditPlan
	// (sort.Slice is not stable on equal start values).
	rwPlan := NewEditPlan(tpl)
	rewriteBoxedReads(tpl, rwPlan, boxedSlots)
	rewriteBoxedWrites(tpl, rwPlan, boxedSlots)
	rwPlan.Apply()

	// Step 4: Insert boxing preamble at position 0.
	// Runs as a separate pass after read/write rewrites are applied,
	// so there are no overlapping edits at position 0.
	preamblePlan := NewEditPlan(tpl)
	insertParameterBoxing(preamblePlan, boxedSlots)
	preamblePlan.Apply()

	// Step 5: Mark captures in sub-templates as boxed.
	markCapturesBoxed(tpl, boxedSlots)
}

// collectBoxedSlots determines which local slots in tpl need boxing.
// A slot needs boxing when it is:
//   - Captured by a sub-template (SourceDepth == 1 in a sub-template's captures)
//   - Mutated anywhere (in the sub-template's Mutated set at the corresponding
//     depth, or by OpStoreLocal at depth=0 in tpl's own bytecodes)
func collectBoxedSlots(tpl *NativeTemplate) map[int]bool {
	// First, find which slots are captured by any sub-template at depth=1.
	capturedSlots := make(map[int]bool)
	// Also track which captured slots are mutated from within sub-templates.
	subMutatedSlots := make(map[int]bool)

	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*NativeTemplate)
		if !ok {
			continue
		}
		subInfo := sub.FreeVarInfo()
		if subInfo == nil {
			continue
		}
		for _, cap := range subInfo.Captures {
			if cap.SourceDepth == 1 {
				capturedSlots[cap.SourceSlot] = true
			}
		}
		// Check if the sub-template (or its children) mutate these
		// variables. From the sub-template's perspective, the variable
		// is at (slot, 1).
		for mutKey := range subInfo.Mutated {
			if mutKey[1] == 1 {
				subMutatedSlots[mutKey[0]] = true
			}
		}
	}

	if len(capturedSlots) == 0 {
		return nil
	}

	// Scan the current template's own bytecodes for OpStoreLocal at depth=0,
	// which indicates the defining scope itself does set! on the variable.
	ownMutatedSlots := make(map[int]bool)
	for _, instr := range tpl.Code() {
		if instr.Op != OpStoreLocal {
			continue
		}
		slot, depth := DecodeLocalIndex(instr.Arg)
		if depth == 0 {
			ownMutatedSlots[slot] = true
		}
	}

	// A slot is boxed if it is captured AND mutated by any path.
	boxedSlots := make(map[int]bool)
	for slot := range capturedSlots {
		if subMutatedSlots[slot] || ownMutatedSlots[slot] {
			boxedSlots[slot] = true
		}
	}
	return boxedSlots
}

// insertParameterBoxing inserts boxing preamble at the top of the template
// for each boxed slot. The preamble wraps the parameter value in a *values.Box:
//
//	OpLoadLocal(slot, 0)   ; load parameter value
//	OpBox                  ; wrap in *values.Box
//	OpPush                 ; push boxed value to eval stack
//	OpStoreLocal(slot, 0)  ; replace parameter with boxed version
func insertParameterBoxing(plan *EditPlan, boxedSlots map[int]bool) {
	// Collect and sort slots for deterministic output.
	slots := sortedSlots(boxedSlots)

	// Build all preamble instructions, 4 per slot.
	preamble := make([]Instruction, 0, len(slots)*4)
	for _, slot := range slots {
		encoded := encodeSlotDepthZero(slot)
		preamble = append(preamble,
			Instruction{Op: OpLoadLocal, Arg: encoded},
			Instruction{Op: OpBox},
			Instruction{Op: OpPush},
			Instruction{Op: OpStoreLocal, Arg: encoded},
		)
	}

	// Insert all preamble instructions before position 0.
	plan.Insert(0, preamble, 0)
}

// rewriteBoxedReads rewrites reads of boxed variables to unbox them.
//
//   - OpLoadLocal(slot, 0) where slot is boxed: replace with
//     OpLoadLocal(slot, 0) + OpUnbox.
//   - OpPushLocal(slot, 0) where slot is boxed: replace with
//     OpLoadLocal(slot,0) + OpUnbox + OpPush.
//
// Both cases use Replace rather than Insert to avoid overlapping edits
// when a read at pc is immediately followed by a write at pc+1.
func rewriteBoxedReads(tpl *NativeTemplate, plan *EditPlan, boxedSlots map[int]bool) {
	for pc, instr := range tpl.Code() {
		slot, depth := DecodeLocalIndex(instr.Arg)
		if depth != 0 || !boxedSlots[slot] {
			continue
		}

		encoded := encodeSlotDepthZero(slot)

		switch instr.Op {
		case OpLoadLocal:
			// Replace LoadLocal with LoadLocal + Unbox.
			plan.Replace(pc, pc+1, []Instruction{
				{Op: OpLoadLocal, Arg: encoded},
				{Op: OpUnbox},
			}, tpl.sourceRefs[pc])

		case OpPushLocal:
			// Replace PushLocal with LoadLocal + Unbox + Push.
			plan.Replace(pc, pc+1, []Instruction{
				{Op: OpLoadLocal, Arg: encoded},
				{Op: OpUnbox},
				{Op: OpPush},
			}, tpl.sourceRefs[pc])
		}
	}
}

// rewriteBoxedWrites replaces writes to boxed variables.
//
// OpStoreLocal(slot, 0) where slot is boxed becomes:
//
//	OpLoadLocal(slot, 0)  ; load the *values.Box into value_reg
//	OpSetBox              ; box.Value = evals.Pop()
func rewriteBoxedWrites(tpl *NativeTemplate, plan *EditPlan, boxedSlots map[int]bool) {
	for pc, instr := range tpl.Code() {
		if instr.Op != OpStoreLocal {
			continue
		}
		slot, depth := DecodeLocalIndex(instr.Arg)
		if depth != 0 || !boxedSlots[slot] {
			continue
		}

		encoded := encodeSlotDepthZero(slot)
		plan.Replace(pc, pc+1, []Instruction{
			{Op: OpLoadLocal, Arg: encoded},
			{Op: OpSetBox},
		}, tpl.sourceRefs[pc])
	}
}

// markCapturesBoxed sets Boxed=true on CaptureEntry values in the template
// tree for boxed variables. Each closure boundary adds depth=1, so a boxed
// slot at depth=0 in the defining template appears at depth=1 in direct
// sub-templates, depth=2 in sub-sub-templates, etc.
//
// The recursion walks the entire sub-template tree because variables
// captured at depth > 1 (through intermediate closures) also carry the
// Box value at runtime and need OpUnbox.
func markCapturesBoxed(tpl *NativeTemplate, boxedSlots map[int]bool) {
	markCapturesBoxedAtDepth(tpl, boxedSlots, 1)
}

func markCapturesBoxedAtDepth(tpl *NativeTemplate, boxedSlots map[int]bool, depth int) {
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*NativeTemplate)
		if !ok {
			continue
		}
		subInfo := sub.FreeVarInfo()
		if subInfo == nil {
			continue
		}
		for i := range subInfo.Captures {
			entry := &subInfo.Captures[i]
			if entry.SourceDepth == depth && boxedSlots[entry.SourceSlot] {
				entry.Boxed = true
			}
		}
		markCapturesBoxedAtDepth(sub, boxedSlots, depth+1)
	}
}

// sortedSlots returns the keys of a map[int]bool in ascending order.
func sortedSlots(m map[int]bool) []int {
	q := make([]int, 0, len(m))
	for k := range m {
		q = append(q, k)
	}
	// Simple insertion sort — slot counts are tiny (typically < 10).
	for i := 1; i < len(q); i++ {
		for j := i; j > 0 && q[j-1] > q[j]; j-- {
			q[j-1], q[j] = q[j], q[j-1]
		}
	}
	return q
}

// encodeSlotDepthZero encodes a local variable reference at depth=0
// using the same bit-packing as EncodeLocalIndex.
func encodeSlotDepthZero(slot int) int32 {
	return int32(slot & 0xFFFF)
}
