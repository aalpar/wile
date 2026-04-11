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
	"sort"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// EditPlan collects non-overlapping bytecode edits for a NativeTemplate.
// Edits are applied in a single pass via Apply, which rewrites code,
// sourceRefs, and branch offsets, then garbage-collects unreferenced
// sideTable entries.
//
// Apply remaps these Arg categories automatically:
//   - PC offsets: Branch, BranchOnFalseValue, SaveContinuation
//   - SideTable indices: Complex (unreferenced entries are GC'd)
//
// These are stable across Apply and not touched:
//   - Literal pool indices: LoadLiteral, LoadGlobal, StoreGlobal, PushLiteral, PushGlobal
//   - Local indices: LoadLocal, StoreLocal, PushLocal
//   - Stack offsets: PeekK
type EditPlan struct {
	tpl   *NativeTemplate
	edits []edit
}

type edit struct {
	start, end int           // [start, end) range in original code to replace
	replace    []Instruction // replacement instructions (nil = pure deletion)
	sourceRef  uint32        // source ref for all replacement instructions
}

// NewEditPlan creates a new edit plan for the given template.
func NewEditPlan(tpl *NativeTemplate) *EditPlan {
	return &EditPlan{tpl: tpl}
}

// AddLiteral adds a value to the template's literal pool, with deduplication.
// Returns the index suitable for use in Instruction.Arg fields that reference
// the literal pool (OpLoadLiteral, OpLoadGlobal, OpStoreGlobal).
func (p *EditPlan) AddLiteral(v values.Value) int32 {
	return int32(p.tpl.MaybeAppendLiteral(v))
}

// Replace marks the range [start, end) for replacement with instrs.
// src is the source reference for all inserted instructions.
func (p *EditPlan) Replace(start, end int, instrs []Instruction, src uint32) {
	p.edits = append(p.edits, edit{
		start:     start,
		end:       end,
		replace:   instrs,
		sourceRef: src,
	})
}

// Delete marks the range [start, end) for removal.
func (p *EditPlan) Delete(start, end int) {
	p.Replace(start, end, nil, 0)
}

// Insert inserts instrs before the instruction at position at.
func (p *EditPlan) Insert(at int, instrs []Instruction, src uint32) {
	p.Replace(at, at, instrs, src)
}

// HasEdits reports whether any edits have been added.
func (p *EditPlan) HasEdits() bool {
	return len(p.edits) > 0
}

// Apply rewrites the template according to all accumulated edits:
//  1. Sorts and validates edits (panics on overlap)
//  2. Builds a PC remap from old positions to new positions
//  3. Fixes branch offsets for surviving original instructions
//  4. Rebuilds code + sourceRefs, splicing in replacements
//  5. Garbage-collects unreferenced sideTable entries, remaps OpComplex.Arg
//
// Returns the net change in instruction count (negative means code shrunk).
func (p *EditPlan) Apply() int {
	if len(p.edits) == 0 {
		return 0
	}

	oldLen := len(p.tpl.code)

	sort.Slice(p.edits, func(i, j int) bool {
		return p.edits[i].start < p.edits[j].start
	})
	validateEdits(p.edits, oldLen)

	remap := buildEditRemap(p.edits, oldLen)
	fixSurvivingBranches(p.tpl.code, p.edits, remap)
	p.tpl.code, p.tpl.sourceRefs = rewriteCode(p.tpl.code, p.tpl.sourceRefs, p.edits)
	gcSideTable(p.tpl)

	return len(p.tpl.code) - oldLen
}

// validateEdits panics if any edits overlap or fall outside [0, codeLen].
func validateEdits(edits []edit, codeLen int) {
	for i, e := range edits {
		if e.start < 0 || e.end > codeLen || e.start > e.end {
			panic(werr.WrapForeignErrorf(werr.ErrIndexOutOfRange, "edit_plan: edit out of bounds (start=%d, end=%d, codeLen=%d)", e.start, e.end, codeLen))
		}
		if i > 0 && e.start < edits[i-1].end {
			panic(werr.WrapForeignErrorf(
				werr.ErrInvalidArgument,
				"edit_plan: overlapping edits: prev=[%d,%d) curr=[%d,%d)",
				edits[i-1].start, edits[i-1].end, e.start, e.end,
			))
		}
	}
}

// buildEditRemap constructs a mapping from old PC positions to new PC
// positions after all edits are applied. The returned slice has length
// codeLen+1, where remap[codeLen] is the new sentinel position (total
// length of the rewritten code).
//
// Positions inside a deleted range [start, end) all map to the new
// position of the replacement's start, so branches targeting any
// instruction in a replaced range land at the replacement.
func buildEditRemap(edits []edit, codeLen int) []int {
	remap := make([]int, codeLen+1)
	// delta tracks the cumulative shift: instructions inserted minus
	// instructions deleted by all edits processed so far. A surviving
	// instruction at old position i lands at new position i+delta.
	delta := 0
	editIdx := 0
	i := 0

	for i <= codeLen {
		if editIdx < len(edits) && i == edits[editIdx].start {
			e := edits[editIdx]

			// Every old position in the replaced range [start, end) maps
			// to the same new position — the start of the replacement block.
			// This is correct for branch targets: a branch into the middle
			// of a replaced sequence lands at the replacement's first instruction.
			newStart := i + delta
			for j := e.start; j < e.end; j++ {
				remap[j] = newStart
			}

			// Update delta: the replacement contributes len(replace) new
			// instructions in place of (end-start) old ones.
			delta += len(e.replace) - (e.end - e.start)

			if e.end > e.start {
				// Replacement or deletion: skip past the consumed range.
				i = e.end
			} else {
				// Pure insertion (start == end): no old instructions are
				// consumed, so position i still exists and shifts by the
				// updated delta.
				remap[i] = i + delta
				i++
			}
			editIdx++
		} else {
			// Surviving instruction — shifted by cumulative delta.
			remap[i] = i + delta
			i++
		}
	}

	return remap
}

// isBranchOp returns true for opcodes whose Arg is a relative PC offset
// that must be adjusted when instructions are removed or inserted.
// Derived from opcodeTable metadata.
func isBranchOp(op OpCode) bool {
	return opcodeTable[op].isBranch
}

// fixSurvivingBranches adjusts branch offsets for original instructions
// that survive (are not inside any edit range). Replacement instructions
// are left as-is — the caller is responsible for their offsets.
func fixSurvivingBranches(code []Instruction, edits []edit, remap []int) {
	inEdit := make([]bool, len(code))
	for _, e := range edits {
		for j := e.start; j < e.end; j++ {
			inEdit[j] = true
		}
	}

	for i := range code {
		if inEdit[i] {
			continue
		}
		if !isBranchOp(code[i].Op) {
			continue
		}
		absTarget := i + int(code[i].Arg)
		code[i].Arg = int32(remap[absTarget] - remap[i])
	}
}

// rewriteCode builds new code and sourceRefs arrays by copying surviving
// segments and splicing in replacement instructions from each edit.
func rewriteCode(code []Instruction, sourceRefs []uint32, edits []edit) ([]Instruction, []uint32) {
	// Compute new length.
	newLen := len(code)
	for _, e := range edits {
		newLen += len(e.replace) - (e.end - e.start)
	}

	newCode := make([]Instruction, 0, newLen)
	newRefs := make([]uint32, 0, newLen)
	prev := 0

	for _, e := range edits {
		// Copy surviving segment before this edit.
		newCode = append(newCode, code[prev:e.start]...)
		newRefs = append(newRefs, sourceRefs[prev:e.start]...)

		// Splice in replacement.
		newCode = append(newCode, e.replace...)
		for range e.replace {
			newRefs = append(newRefs, e.sourceRef)
		}
		prev = e.end
	}

	// Copy trailing segment after last edit.
	newCode = append(newCode, code[prev:]...)
	newRefs = append(newRefs, sourceRefs[prev:]...)

	return newCode, newRefs
}

// gcSideTable removes unreferenced sideTable entries and remaps
// OpComplex.Arg values to the compacted indices.
func gcSideTable(tpl *NativeTemplate) {
	if len(tpl.sideTable) == 0 {
		return
	}

	// Mark which entries are still referenced.
	referenced := make([]bool, len(tpl.sideTable))
	for _, instr := range tpl.code {
		if instr.Op == OpComplex {
			referenced[instr.Arg] = true
		}
	}

	// Check if GC is needed.
	deadCount := 0
	for _, ref := range referenced {
		if !ref {
			deadCount++
		}
	}
	if deadCount == 0 {
		return
	}

	// Build old index → new index mapping.
	newIdx := make([]int32, len(tpl.sideTable))
	w := int32(0)
	for i, ref := range referenced {
		if ref {
			newIdx[i] = w
			w++
		}
	}

	// Remap OpComplex.Arg in code.
	for i := range tpl.code {
		if tpl.code[i].Op == OpComplex {
			tpl.code[i].Arg = newIdx[tpl.code[i].Arg]
		}
	}

	// Compact sideTable in place.
	j := 0
	for i, ref := range referenced {
		if ref {
			tpl.sideTable[j] = tpl.sideTable[i]
			j++
		}
	}
	tpl.sideTable = tpl.sideTable[:j]
}
