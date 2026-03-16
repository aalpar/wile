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
	"github.com/aalpar/wile/werr"
)

// FlattenClosures rewrites free-variable references in closure bodies
// from OpLoadLocal(slot, depth>0) to OpLoadFreeVar(closureSlot), and
// rewrites OpMakeClosure to OpMakeFlatClosure for templates with captures.
//
// This is Pass 3 of the flat closure pipeline. It modifies bytecodes
// in the CAPTURING template (the closure body).
//
// enclosingInfo is the FreeVarInfo of the enclosing template, used to
// resolve FromFreeVars for captures with SourceDepth > 1. Pass nil for
// top-level templates.
//
// Execution order: top-down (outer templates before inner), so that
// enclosingInfo is available when processing children.
func FlattenClosures(tpl *NativeTemplate, enclosingInfo *FreeVarInfo) {
	info := tpl.FreeVarInfo()

	// Step 1: Resolve FromFreeVars on captures.
	if info != nil && len(info.Captures) > 0 {
		resolveFromFreeVars(info, enclosingInfo)
	}

	// Step 2: Build lookup map and rewrite bytecodes.
	if info != nil && len(info.Captures) > 0 {
		lookup := buildFreeVarLookup(info)
		rewriteFreeVarReferences(tpl, lookup)

		// Step 2.5: Insert OpUnbox after OpLoadFreeVar for boxed captures.
		// rewriteFreeVarReferences already handled OpStoreLocal(depth>0) by
		// emitting OpLoadFreeVar + OpSetBox. For reads, we now need to unbox
		// the *values.Box loaded by OpLoadFreeVar.
		rewriteBoxedFreeVarReads(tpl, info)
	}

	// Step 3: Rewrite OpMakeClosure → OpMakeFlatClosure for sub-templates
	// that have captures.
	rewriteMakeClosureToFlat(tpl)

	// Step 4: Recurse into sub-templates (top-down).
	// Skip sub-templates already processed by a nested compileClosureBody call.
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*NativeTemplate)
		if !ok {
			continue
		}
		if sub.flatClosuresDone {
			continue
		}
		FlattenClosures(sub, info)
	}
}

// resolveFromFreeVars resolves the FromFreeVars flag and adjusts SourceSlot
// for captures that pass through the enclosing scope's freeVars array.
//
// For captures at SourceDepth == 1: the value comes from the enclosing
// scope's local bindings, so FromFreeVars = false.
//
// For captures at SourceDepth > 1: the value is itself a free variable of
// the enclosing scope. FromFreeVars is set to true, and SourceSlot is
// rewritten to the enclosing scope's ClosureSlot for that variable.
func resolveFromFreeVars(info *FreeVarInfo, enclosingInfo *FreeVarInfo) {
	for i := range info.Captures {
		entry := &info.Captures[i]
		if entry.SourceDepth == 1 {
			entry.FromFreeVars = false
			continue
		}
		// SourceDepth > 1: the variable passes through the enclosing scope.
		if enclosingInfo == nil {
			continue
		}
		entry.FromFreeVars = true
		// Find the enclosing template's capture entry for this variable.
		// From the current template's perspective, the variable is at
		// (SourceSlot, SourceDepth). From the enclosing template's
		// perspective, it is at (SourceSlot, SourceDepth-1).
		encSlot := findEnclosingClosureSlot(enclosingInfo, entry.SourceSlot, entry.SourceDepth-1)
		if encSlot < 0 {
			panic(werr.WrapForeignErrorf(
				werr.ErrNoSuchBinding,
				"FlattenClosures: no enclosing capture for (slot=%d, depth=%d)",
				entry.SourceSlot, entry.SourceDepth-1,
			))
		}
		entry.SourceSlot = encSlot
	}
}

// findEnclosingClosureSlot searches the enclosing template's FreeVarInfo
// for a capture matching (slot, depth) and returns its ClosureSlot.
// Returns -1 if not found.
func findEnclosingClosureSlot(enclosingInfo *FreeVarInfo, slot, depth int) int {
	for _, c := range enclosingInfo.Captures {
		if c.SourceSlot == slot && c.SourceDepth == depth {
			return c.ClosureSlot
		}
	}
	return -1
}

// buildFreeVarLookup builds a map from (slot, depth) to closureSlot for
// the template's FreeVarInfo captures.
func buildFreeVarLookup(info *FreeVarInfo) map[[2]int]int {
	q := make(map[[2]int]int, len(info.Captures))
	for _, c := range info.Captures {
		q[[2]int{c.SourceSlot, c.SourceDepth}] = c.ClosureSlot
	}
	return q
}

// rewriteFreeVarReferences rewrites OpLoadLocal(slot, depth>0),
// OpPushLocal(slot, depth>0), and OpStoreLocal(slot, depth>0) to flat
// closure operations using an EditPlan.
//
// Load/Push: rewritten to OpLoadFreeVar(closureSlot).
// Store: rewritten to OpLoadFreeVar(closureSlot) + OpSetBox. All
// cross-scope stores must target boxed variables (the boxing criterion
// in InsertBoxes guarantees this).
func rewriteFreeVarReferences(tpl *NativeTemplate, lookup map[[2]int]int) {
	plan := NewEditPlan(tpl)

	for pc, instr := range tpl.Code() {
		switch instr.Op {
		case OpLoadLocal:
			slot, depth := DecodeLocalIndex(instr.Arg)
			if depth == 0 {
				continue
			}
			closureSlot, ok := lookup[[2]int{slot, depth}]
			if !ok {
				continue
			}
			plan.Replace(pc, pc+1, []Instruction{
				{Op: OpLoadFreeVar, Arg: int32(closureSlot)},
			}, tpl.sourceRefs[pc])

		case OpPushLocal:
			slot, depth := DecodeLocalIndex(instr.Arg)
			if depth == 0 {
				continue
			}
			closureSlot, ok := lookup[[2]int{slot, depth}]
			if !ok {
				continue
			}
			plan.Replace(pc, pc+1, []Instruction{
				{Op: OpLoadFreeVar, Arg: int32(closureSlot)},
				{Op: OpPush},
			}, tpl.sourceRefs[pc])

		case OpStoreLocal:
			slot, depth := DecodeLocalIndex(instr.Arg)
			if depth == 0 {
				continue
			}
			closureSlot, ok := lookup[[2]int{slot, depth}]
			if !ok {
				continue
			}
			// Cross-scope stores always target boxed variables.
			// Rewrite: OpLoadFreeVar(closureSlot) loads the *values.Box,
			// then OpSetBox stores evals.Pop() into it.
			plan.Replace(pc, pc+1, []Instruction{
				{Op: OpLoadFreeVar, Arg: int32(closureSlot)},
				{Op: OpSetBox},
			}, tpl.sourceRefs[pc])
		}
	}

	plan.Apply()
}

// rewriteBoxedFreeVarReads inserts OpUnbox after OpLoadFreeVar instructions
// that reference boxed captures. This is the capturing-scope counterpart of
// rewriteBoxedReads in pass_box_insertion.go (which handles the defining scope).
//
// After rewriteFreeVarReferences, boxed reads appear as:
//
//	OpLoadFreeVar(closureSlot)     ; loads *values.Box
//
// This pass rewrites them to:
//
//	OpLoadFreeVar(closureSlot)     ; loads *values.Box
//	OpUnbox                        ; extracts box.Value
//
// Non-boxed captures are left unchanged. OpSetBox sequences (from
// rewriteFreeVarReferences handling StoreLocal) are also left unchanged —
// they correctly load the box and set its value.
func rewriteBoxedFreeVarReads(tpl *NativeTemplate, info *FreeVarInfo) {
	// Build a set of closure slots that are boxed.
	boxedSlots := make(map[int32]bool)
	for _, c := range info.Captures {
		if c.Boxed {
			boxedSlots[int32(c.ClosureSlot)] = true
		}
	}
	if len(boxedSlots) == 0 {
		return
	}

	plan := NewEditPlan(tpl)
	code := tpl.Code()

	for pc, instr := range code {
		if instr.Op != OpLoadFreeVar {
			continue
		}
		if !boxedSlots[instr.Arg] {
			continue
		}
		// Skip if the next instruction is OpSetBox — this is a write sequence
		// emitted by rewriteFreeVarReferences for OpStoreLocal(depth>0).
		if pc+1 < len(code) && code[pc+1].Op == OpSetBox {
			continue
		}
		// Insert OpUnbox after OpLoadFreeVar.
		plan.Replace(pc, pc+1, []Instruction{
			{Op: OpLoadFreeVar, Arg: instr.Arg},
			{Op: OpUnbox},
		}, tpl.sourceRefs[pc])
	}

	plan.Apply()
}

// rewriteMakeClosureToFlat scans for OpMakeClosure instructions and replaces
// them with OpMakeFlatClosure when the child template has captures (non-empty
// FreeVarInfo).
//
// The instruction stream is otherwise unchanged — OpMakeFlatClosure pops
// both the env and the template from the stack, just like OpMakeClosure.
// The difference is how the closure is constructed: MakeFlatClosure
// additionally populates the freeVars array from the creating scope's
// bindings/freeVars.
func rewriteMakeClosureToFlat(tpl *NativeTemplate) {
	plan := NewEditPlan(tpl)
	code := tpl.Code()

	for pc := range code {
		if code[pc].Op != OpMakeClosure {
			continue
		}

		// Find the template literal index by scanning backward.
		tplLitIdx := findTemplateLiteralIndex(code, pc)
		if tplLitIdx < 0 {
			continue
		}

		// Look up the child template from the literal pool.
		lits := tpl.Literals()
		if int(tplLitIdx) >= len(lits) {
			continue
		}
		childTpl, ok := lits[tplLitIdx].(*NativeTemplate)
		if !ok {
			continue
		}

		// Only rewrite if the child template has captures.
		childInfo := childTpl.FreeVarInfo()
		if childInfo == nil || len(childInfo.Captures) == 0 {
			continue
		}

		// Replace only the MakeClosure instruction with MakeFlatClosure.
		// The env and template pushes remain on the stack.
		plan.Replace(pc, pc+1, []Instruction{
			{Op: OpMakeFlatClosure},
		}, tpl.sourceRefs[pc])
	}

	plan.Apply()
}

// findTemplateLiteralIndex scans backward from an OpMakeClosure at code[pc]
// to find the template literal index. Handles both unfused and peephole-fused
// instruction sequences.
//
// Returns the literal index, or -1 if the pattern is not recognized.
func findTemplateLiteralIndex(code []Instruction, pc int) int32 {
	// Pattern A (unfused, 5 instructions):
	//   pc-4: LoadLiteral(tplIdx)
	//   pc-3: Push
	//   pc-2: LoadLiteral(envIdx)
	//   pc-1: Push
	//   pc:   MakeClosure
	if pc >= 4 &&
		code[pc-4].Op == OpLoadLiteral &&
		code[pc-3].Op == OpPush &&
		code[pc-2].Op == OpLoadLiteral &&
		code[pc-1].Op == OpPush {
		return code[pc-4].Arg
	}

	// Pattern B (fully fused, 3 instructions):
	//   pc-2: PushLiteral(tplIdx)
	//   pc-1: PushLiteral(envIdx)
	//   pc:   MakeClosure
	if pc >= 2 &&
		code[pc-2].Op == OpPushLiteral &&
		code[pc-1].Op == OpPushLiteral {
		return code[pc-2].Arg
	}

	// Pattern C (tpl unfused, env fused, 4 instructions):
	//   pc-3: LoadLiteral(tplIdx)
	//   pc-2: Push
	//   pc-1: PushLiteral(envIdx)
	//   pc:   MakeClosure
	if pc >= 3 &&
		code[pc-3].Op == OpLoadLiteral &&
		code[pc-2].Op == OpPush &&
		code[pc-1].Op == OpPushLiteral {
		return code[pc-3].Arg
	}

	// Pattern D (tpl fused, env unfused, 4 instructions):
	//   pc-3: PushLiteral(tplIdx)
	//   pc-2: LoadLiteral(envIdx)
	//   pc-1: Push
	//   pc:   MakeClosure
	if pc >= 3 &&
		code[pc-3].Op == OpPushLiteral &&
		code[pc-2].Op == OpLoadLiteral &&
		code[pc-1].Op == OpPush {
		return code[pc-3].Arg
	}

	return -1
}
